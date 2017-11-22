module EEChat exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
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


-- MODEL

type alias Model =
    { nick : String
    , nickAccepted : Bool
    , newMessage : String
    , chatBuffer: String
    , phxSocket: Phoenix.Socket.Socket Msg
    }

model : Model

model =
    { nick = ""
    , nickAccepted = False
    , newMessage = ""
    , chatBuffer = ""
    , phxSocket = Phoenix.Socket.init "http://localhost:4000/socket/websocket"
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Phoenix.Socket.listen model.phxSocket PhoenixMsg

-- UPDATE

type Msg
    = AcceptNick
    | AcceptNewMessage
    | UpdateNick String
    | UpdateNewMessage String
    | PhoenixMsg (Phoenix.Socket.Msg Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNick nick ->
            { model | nick = nick } ! []
        UpdateNewMessage str ->
            { model | newMessage = str } ! []
        AcceptNick ->
            { model | nickAccepted = True, chatBuffer = model.chatBuffer ++ "\n" ++ "* " ++ model.nick ++ " joined the chat *"}
            ! []
        AcceptNewMessage ->
            { model
                | chatBuffer = model.chatBuffer ++ "\n" ++ model.nick ++ "> " ++  model.newMessage
                , newMessage = "" }
            ! []
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
        , input [ value model.newMessage, onInput UpdateNewMessage, onEnter AcceptNewMessage ] []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)
