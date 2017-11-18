module EEChat exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

main =
    Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
    { nick : String
    , nickAccepted : Bool
    , newMessage : String
    , chatBuffer: String
    }

model : Model

model =
    { nick = ""
    , nickAccepted = False
    , newMessage = ""
    , chatBuffer = "foo> lorem ipsum\nbar> heppa heppa zign"
    }


-- UPDATE

type Msg = AcceptNick
         | AcceptNewMessage
         | UpdateNick String
         | UpdateNewMessage String

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateNick nick ->
            { model | nick = nick }
        UpdateNewMessage str ->
            { model | newMessage = str }
        AcceptNick ->
            { model | nickAccepted = True, chatBuffer = model.chatBuffer ++ "\n" ++ "* " ++ model.nick ++ " joined the chat *"}
        AcceptNewMessage ->
            { model
                | chatBuffer = model.chatBuffer ++ "\n" ++ model.nick ++ "> " ++  model.newMessage
                , newMessage = "" }

-- VIEW

view : Model -> Html Msg

view model =
    if model.nickAccepted == False then
        span []
            [ input [ placeholder "nick", onInput UpdateNick, onEnter AcceptNick] []
            , button [ onClick AcceptNick ] [ text "Enter" ]
            ]
    else
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
