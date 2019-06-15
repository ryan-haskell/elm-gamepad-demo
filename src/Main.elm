port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Encode as Json


port outgoing : Json.Value -> Cmd msg


port incoming : (Json.Value -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { gamepad : GamepadState
    }


type GamepadState
    = ReadyToConnect
    | Connected XboxGamepad
    | Disconnected


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ReadyToConnect, Cmd.none )



-- UPDATE


type Msg
    = Msg
    | FromJs Json.Value
    | RequestGamepad


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )

        FromJs json ->
            case D.decodeValue jsonMessageDecoder json of
                Ok message ->
                    case message of
                        GamepadConnected gamepad ->
                            ( { model | gamepad = Connected (toXboxGamepad gamepad) }
                            , Cmd.none
                            )

                        GamepadDisconnected _ ->
                            ( { model | gamepad = Disconnected }
                            , Cmd.none
                            )

                Err reason ->
                    let
                        _ =
                            Debug.log "error" (D.errorToString reason)
                    in
                    ( model, Cmd.none )

        RequestGamepad ->
            case model.gamepad of
                Connected gamepad ->
                    ( model
                    , outgoingMessage (PollGamepad gamepad.id)
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gamepad of
        Connected _ ->
            Sub.batch
                [ incoming FromJs
                , Browser.Events.onAnimationFrame (always RequestGamepad)
                ]

        _ ->
            incoming FromJs


type alias XboxGamepad =
    { id : String
    , buttons : Buttons
    , bumpers : Bumpers
    , triggers : Triggers
    , joysticks : Joysticks
    }


type alias Buttons =
    { a : Bool
    , b : Bool
    , x : Bool
    , y : Bool
    }


type alias Bumpers =
    { left : Bool
    , right : Bool
    }


type alias Triggers =
    { left : Float
    , right : Float
    }


type alias Joysticks =
    { left : ( HorizontalAxis, VerticalAxis )
    , right : ( HorizontalAxis, VerticalAxis )
    }


horizontal : Float -> HorizontalAxis
horizontal value =
    if value < -0.33 then
        Left

    else if value > 0.33 then
        Right

    else
        CenterX


vertical : Float -> VerticalAxis
vertical value =
    if value < -0.33 then
        Up

    else if value > 0.33 then
        Down

    else
        CenterY


type HorizontalAxis
    = Left
    | CenterX
    | Right


type VerticalAxis
    = Up
    | CenterY
    | Down


toXboxGamepad : Gamepad -> XboxGamepad
toXboxGamepad gamepad =
    let
        buttons =
            Array.fromList gamepad.buttons

        axes =
            Array.fromList gamepad.axes

        isPressed index =
            Array.get index buttons
                |> Maybe.map .pressed
                |> Maybe.withDefault False

        valueOf index =
            Array.get index buttons
                |> Maybe.map .value
                |> Maybe.withDefault 0

        horizontalValueOf index =
            Array.get index axes
                |> Maybe.withDefault 0
                |> horizontal

        verticalValueOf index =
            Array.get index axes
                |> Maybe.withDefault 0
                |> vertical
    in
    XboxGamepad
        gamepad.id
        (Buttons
            (isPressed 0)
            (isPressed 1)
            (isPressed 2)
            (isPressed 3)
        )
        (Bumpers
            (isPressed 4)
            (isPressed 5)
        )
        (Triggers
            (valueOf 6)
            (valueOf 7)
        )
        (Joysticks
            ( horizontalValueOf 0, verticalValueOf 1 )
            ( horizontalValueOf 2, verticalValueOf 3 )
        )



-- JSON messaging


type JsonMessage
    = GamepadConnected Gamepad
    | GamepadDisconnected Gamepad


outgoingMessage : OutgoingMessage -> Cmd msg
outgoingMessage msg =
    outgoing <|
        case msg of
            PollGamepad id ->
                Json.object
                    [ ( "action", Json.string "pollGamepad" )
                    , ( "data", Json.string id )
                    ]


type OutgoingMessage
    = PollGamepad String


type alias Gamepad =
    { id : String
    , axes : List Float
    , buttons : List GamepadButton
    }


type alias GamepadButton =
    { value : Float
    , pressed : Bool
    }


jsonMessageDecoder : Decoder JsonMessage
jsonMessageDecoder =
    D.oneOf
        [ withMessageType "gamepadConnected"
            (D.map GamepadConnected (D.field "gamepad" gamepadDecoder))
        , withMessageType "gamepadDisconnected"
            (D.map GamepadDisconnected (D.field "gamepad" gamepadDecoder))
        ]


withMessageType : String -> Decoder a -> Decoder a
withMessageType type_ decoder =
    D.map2 (\t event -> event)
        (D.field "type" D.string
            |> D.andThen
                (\t ->
                    if t == type_ then
                        D.succeed t

                    else
                        D.fail ("Was not type: " ++ type_)
                )
        )
        (D.field "event" decoder)


gamepadDecoder : Decoder Gamepad
gamepadDecoder =
    D.map3 Gamepad
        (D.field "id" D.string)
        (D.field "axes" (D.list D.float))
        (D.field "buttons" (D.list gamepadButtonDecoder))


gamepadButtonDecoder : Decoder GamepadButton
gamepadButtonDecoder =
    D.map2 GamepadButton
        (D.field "value" D.float)
        (D.field "pressed" D.bool)



-- VIEW


view : Model -> Html Msg
view model =
    case model.gamepad of
        ReadyToConnect ->
            h1 [] [ text "Press A to get started." ]

        Connected gamepad ->
            div []
                [ h1 [] [ text "Gamepad connected!" ]
                , h2 [] [ text (Debug.toString gamepad) ]
                ]

        Disconnected ->
            h1 [] [ text "Oops! Please reconnect your controller." ]
