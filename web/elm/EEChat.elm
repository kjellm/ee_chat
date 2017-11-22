module EEChat exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD exposing (field)
import Json.Encode as JE

import Platform.Cmd
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- CONSTANTS


socketServer =
    "ws://localhost:4000/socket/websocket"


-- MODEL

type Msg
    = AcceptNick
    | SendMessage
    | UpdateNick String
    | UpdateNewMessage String
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveChatMessage JE.Value


type alias Model =
    { nick : String
    , nickAccepted : Bool
    , newMessage : String
    , chatBuffer: String
    , phxSocket: Phoenix.Socket.Socket Msg
    }

initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:message" "room:lobby" ReceiveChatMessage


model : Model
model =
    { nick = ""
    , nickAccepted = False
    , newMessage = ""
    , chatBuffer = ""
    , phxSocket = initPhxSocket
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Phoenix.Socket.listen model.phxSocket PhoenixMsg


-- COMMANDS


-- PHOENIX STUFF


type alias ChatMessage =
    { nick : String
    , body : String
    }


chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
    JD.map2 ChatMessage
        (field "nick" JD.string)
        (field "body" JD.string)


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNick nick ->
            { model | nick = nick } ! []
        UpdateNewMessage str ->
            { model | newMessage = str } ! []
        AcceptNick ->
            let
                channel =
                    Phoenix.Channel.init "room:lobby"
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
                ( { model | phxSocket = phxSocket, nickAccepted = True }
                , Cmd.map PhoenixMsg phxCmd
                )
        SendMessage ->
            let
                payload =
                    (JE.object [ ( "nick", JE.string model.nick ), ( "body", JE.string model.newMessage ) ])

                push_ =
                    Phoenix.Push.init "new:message" "room:lobby"
                        |> Phoenix.Push.withPayload payload

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push_ model.phxSocket
            in
                ( { model
                    | newMessage = ""
                    , phxSocket = phxSocket
                  }
                , Cmd.map PhoenixMsg phxCmd
                )
        ReceiveChatMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    { model
                        | chatBuffer = model.chatBuffer ++ "\n" ++ chatMessage.nick ++ "> " ++  chatMessage.body }
                    ! []
                Err error ->
                    ( model, Cmd.none )
        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )



-- VIEW

view : Model -> Html Msg
view model =
    if model.nickAccepted == False then
        loginView model
    else
        chatView model

loginView model =
    span []
        [ input [ placeholder "nick", onInput UpdateNick, onEnter AcceptNick] []
        , button [ onClick AcceptNick ] [ text "Enter" ]
        ]

chatView model =
    div []
        [ pre [] [text model.chatBuffer ]
        , input [ value model.newMessage, onInput UpdateNewMessage, onEnter SendMessage ] []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                JD.succeed msg
            else
                JD.fail "not ENTER"
    in
        on "keydown" (JD.andThen isEnter keyCode)
