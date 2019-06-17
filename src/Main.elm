port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
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
    , dpad : Dpad
    , joysticks : Joysticks
    }


type alias Buttons =
    { a : Bool
    , b : Bool
    , x : Bool
    , y : Bool
    , back : Bool
    , start : Bool
    , home : Bool
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
    { left : Joystick
    , right : Joystick
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

        verticalValueOf index =
            Array.get index axes
                |> Maybe.withDefault 0
    in
    XboxGamepad
        gamepad.id
        (Buttons
            (isPressed 0)
            (isPressed 1)
            (isPressed 2)
            (isPressed 3)
            (isPressed 8)
            (isPressed 9)
            (isPressed 16)
        )
        (Bumpers
            (isPressed 4)
            (isPressed 5)
        )
        (Triggers
            (valueOf 6)
            (valueOf 7)
        )
        (Dpad
            (isPressed 12)
            (isPressed 13)
            (isPressed 14)
            (isPressed 15)
        )
        (Joysticks
            (Joystick
                (isPressed 10)
                (horizontalValueOf 0)
                (verticalValueOf 1)
            )
            (Joystick
                (isPressed 11)
                (horizontalValueOf 2)
                (verticalValueOf 3)
            )
        )


type alias Dpad =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    }


type alias Joystick =
    { isPressed : Bool
    , x : Float
    , y : Float
    }



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
                , div [ style "display" "flex" ]
                    [ div [ style "margin-right" "2rem" ]
                        [ div []
                            [ h2 [] [ text "Axis" ]
                            , viewAxis "Left Joystick" gamepad.joysticks.left
                            , viewAxis "Right Joystick" gamepad.joysticks.right
                            ]
                        , div []
                            [ h2 [] [ text "Buttons" ]
                            , viewButton "A" gamepad.buttons.a
                            , viewButton "B" gamepad.buttons.b
                            , viewButton "X" gamepad.buttons.x
                            , viewButton "Y" gamepad.buttons.y
                            , viewButton "Back" gamepad.buttons.back
                            , viewButton "Start" gamepad.buttons.start
                            , viewButton "Home" gamepad.buttons.home
                            ]
                        ]
                    , div []
                        [ div []
                            [ h2 [] [ text "Bumpers" ]
                            , viewButton "Left Bumper" gamepad.bumpers.left
                            , viewButton "Right Bumper" gamepad.bumpers.right
                            ]
                        , div []
                            [ h2 [] [ text "Triggers" ]
                            , viewTrigger "Left Trigger" gamepad.triggers.left
                            , viewTrigger "Right Trigger" gamepad.triggers.right
                            ]
                        , div []
                            [ h2 [] [ text "Dpad" ]
                            , viewButton "Up" gamepad.dpad.up
                            , viewButton "Down" gamepad.dpad.down
                            , viewButton "Left" gamepad.dpad.left
                            , viewButton "Right" gamepad.dpad.right
                            ]
                        ]
                    ]
                ]

        Disconnected ->
            h1 [] [ text "Oops! Please reconnect your controller." ]


viewButton : String -> Bool -> Html msg
viewButton label_ isPressed =
    p []
        [ text
            (label_
                ++ ": "
                ++ (if isPressed then
                        "✅"

                    else
                        "❌"
                   )
            )
        ]


viewTrigger : String -> Float -> Html msg
viewTrigger label_ value_ =
    p []
        [ text (label_ ++ ": ")
        , progress [ value (String.fromFloat value_) ] []
        ]


viewAxis : String -> Joystick -> Html msg
viewAxis label_ { isPressed, x, y } =
    let
        emoji =
            if isPressed then
                "😃"

            else
                "🙂"

        toPercent val =
            String.fromFloat (val * 50 + 50) ++ "%"
    in
    p
        []
        [ text (label_ ++ ": ")
        , span
            [ style "position" "relative"
            , style "display" "inline-block"
            , style "width" "25px"
            , style "height" "25px"
            , style "marging" "1rem"
            , style "border" "solid 1px black"
            , style "border-radius" "50%"
            ]
            [ span
                [ style "position" "absolute"
                , style "top" (toPercent y)
                , style "left" (toPercent x)
                , style "transform" "translate(-50%, -50%)"
                ]
                [ text emoji ]
            ]
        ]


directionToAngle : ( HorizontalAxis, VerticalAxis ) -> Maybe Int
directionToAngle ( h, v ) =
    case ( h, v ) of
        ( Right, CenterY ) ->
            Just 0

        ( Right, Up ) ->
            Just 1

        ( CenterX, Up ) ->
            Just 2

        ( Left, Up ) ->
            Just 3

        ( Left, CenterY ) ->
            Just 4

        ( Left, Down ) ->
            Just 5

        ( CenterX, Down ) ->
            Just 6

        ( Right, Down ) ->
            Just 7

        ( CenterX, CenterY ) ->
            Nothing
