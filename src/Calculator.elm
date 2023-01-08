module Calculator exposing (main)

import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, layout, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)



-- STYLING


type Style
    = Windows10
    | Windows11


style : Style
style =
    Windows10


backgroundColor : Element.Attribute Msg
backgroundColor =
    case style of
        Windows10 ->
            Background.color (rgb255 230 230 230)

        Windows11 ->
            Background.color (rgb255 243 242 249)


numberButtonStyle : List (Element.Attribute Msg)
numberButtonStyle =
    case style of
        Windows10 ->
            [ Background.color (rgb255 250 250 250) ]

        Windows11 ->
            [ Background.color (rgb255 252 247 252)
            , Border.rounded 5
            ]


operationButtonStyle : List (Element.Attribute Msg)
operationButtonStyle =
    case style of
        Windows10 ->
            [ Background.color (rgb255 240 240 240) ]

        Windows11 ->
            [ Background.color (rgb255 252 247 250)
            , Border.rounded 5
            ]


equalButtonStyle : List (Element.Attribute Msg)
equalButtonStyle =
    case style of
        Windows10 ->
            operationButtonStyle

        Windows11 ->
            [ Background.color (rgb255 117 87 48)
            , Border.rounded 5
            ]



-- TYPES


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide
    | Dot
    | Equal
    | Clear


type Mode
    = InputNum1
    | InputNum1Decimal
    | InputNum2
    | InputNum2Decimal
    | Done


type Msg
    = NumPressed Int
    | OperPressed Operation


type alias Model =
    { num1 : Maybe Float
    , num2 : Maybe Float
    , mode : Mode
    , operation : Operation
    , result : Maybe Float
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- MODEL


initialModel : Model
initialModel =
    Model Nothing Nothing InputNum1 Clear Nothing



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumPressed num ->
            case model.mode of
                InputNum1 ->
                    { model | num1 = appendIntToNum model.num1 num }

                InputNum1Decimal ->
                    { model | num1 = appendIntToDecimal model.num1 num }

                InputNum2 ->
                    { model | num2 = appendIntToNum model.num2 num }

                InputNum2Decimal ->
                    { model | num2 = appendIntToDecimal model.num2 num }

                Done ->
                    model

        OperPressed oper ->
            case oper of
                Equal ->
                    if model.num1 == Nothing || model.num2 == Nothing then
                        model

                    else
                        let
                            num1 =
                                Maybe.withDefault 0 <| model.num1

                            num2 =
                                Maybe.withDefault 0 <| model.num2
                        in
                        { model
                            | mode = Done
                            , result =
                                case model.operation of
                                    Add ->
                                        Just (num1 + num2)

                                    Subtract ->
                                        Just (num1 - num2)

                                    Multiply ->
                                        Just (num1 * num2)

                                    Divide ->
                                        Just (num1 / num2)

                                    _ ->
                                        Nothing
                        }

                Clear ->
                    { model
                        | num1 = Nothing
                        , num2 = Nothing
                        , mode = InputNum1
                        , operation = Clear
                        , result = Nothing
                    }

                Dot ->
                    case model.mode of
                        Done ->
                            model

                        InputNum1 ->
                            { model | mode = InputNum1Decimal }

                        InputNum1Decimal ->
                            model

                        InputNum2 ->
                            { model | mode = InputNum2Decimal }

                        InputNum2Decimal ->
                            model

                Add ->
                    if model.mode == Done then
                        model

                    else
                        { model
                            | operation = Add
                            , mode = InputNum2
                        }

                Subtract ->
                    if model.mode == Done then
                        model

                    else
                        { model
                            | operation = Subtract
                            , mode = InputNum2
                        }

                Multiply ->
                    if model.mode == Done then
                        model

                    else
                        { model
                            | operation = Multiply
                            , mode = InputNum2
                        }

                Divide ->
                    if model.mode == Done then
                        model

                    else
                        { model
                            | operation = Divide
                            , mode = InputNum2
                        }


appendIntToNum : Maybe Float -> Int -> Maybe Float
appendIntToNum currentNum newDigit =
    case currentNum of
        Nothing ->
            Just (toFloat newDigit)

        Just num ->
            String.fromFloat num
                ++ String.fromInt newDigit
                |> String.toFloat


appendIntToDecimal : Maybe Float -> Int -> Maybe Float
appendIntToDecimal currentNum newDigit =
    case currentNum of
        Nothing ->
            Just (toFloat newDigit / 10)

        Just num ->
            String.fromFloat num
                ++ "."
                ++ String.fromInt newDigit
                |> String.toFloat



-- VIEW


view : Model -> Html Msg
view model =
    let
        buttonSpacing =
            spacing 3
    in
    layout
        [ Font.family
            [ Font.external
                { name = "Open Sans"
                , url = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400&display=swap"
                }
            , Font.sansSerif
            ]
        ]
    <|
        el
            [ Border.width 1
            , Border.rounded 5
            , backgroundColor
            , padding 20
            , centerX
            , centerY
            ]
        <|
            column [ spacing 20 ]
                [ resultsArea model
                , column [ buttonSpacing ]
                    [ row [ buttonSpacing ]
                        [ operationButton Clear
                        , operationButton Clear
                        , operationButton Clear
                        , operationButton Clear
                        ]
                    , row [ buttonSpacing ]
                        [ operationButton Clear
                        , operationButton Clear
                        , operationButton Clear
                        , operationButton Divide
                        ]
                    , row [ buttonSpacing ]
                        [ numberButton 7
                        , numberButton 8
                        , numberButton 9
                        , operationButton Multiply
                        ]
                    , row [ buttonSpacing ]
                        [ numberButton 4
                        , numberButton 5
                        , numberButton 6
                        , operationButton Subtract
                        ]
                    , row [ buttonSpacing ]
                        [ numberButton 1
                        , numberButton 2
                        , numberButton 3
                        , operationButton Add
                        ]
                    , row [ buttonSpacing ]
                        [ operationButton Clear
                        , numberButton 0
                        , operationButton Dot
                        , operationButton Equal
                        ]
                    ]
                ]


resultsArea : Model -> Element Msg
resultsArea model =
    column
        [ width fill, spacing 10, padding 30 ]
        [ el
            [ alignRight
            , padding 5
            , Font.size 20
            ]
          <|
            text <|
                compileExpression model
        , el
            [ Font.alignRight
            , width fill
            , height <| px 70
            , Font.size 60
            , padding 10
            ]
          <|
            text
                (if model.result == Nothing then
                    ""

                 else
                    String.fromFloat <| Maybe.withDefault 0 model.result
                )
        ]


operationAsString : Operation -> String
operationAsString oper =
    case oper of
        Add ->
            "+"

        Subtract ->
            "−"

        Multiply ->
            "×"

        Divide ->
            "÷"

        Dot ->
            "."

        Equal ->
            "="

        Clear ->
            "C"


calcButton : Msg -> Element Msg -> List (Element.Attribute Msg) -> Element Msg
calcButton msg labelText customAttrs =
    let
        defaultAttrs =
            [ width <| px 110
            , height <| px 70
            , Font.size 30
            ]

        finalAttrs =
            List.append defaultAttrs customAttrs
    in
    button
        finalAttrs
        { onPress = Just msg
        , label = labelText
        }


numberButton : Int -> Element Msg
numberButton num =
    let
        labelText =
            el [ centerX, centerY ] (text <| String.fromInt num)
    in
    calcButton (NumPressed num) labelText numberButtonStyle


operationButton : Operation -> Element Msg
operationButton oper =
    let
        labelText =
            el
                [ centerX
                , centerY
                , Font.hairline
                ]
                (text <| operationAsString oper)

        customAttrs =
            case oper of
                Equal ->
                    equalButtonStyle

                _ ->
                    operationButtonStyle
    in
    calcButton (OperPressed oper) labelText customAttrs


compileExpression : Model -> String
compileExpression model =
    let
        num1 =
            case model.num1 of
                Nothing ->
                    ""

                Just num ->
                    String.fromFloat num

        num2 =
            case model.num2 of
                Nothing ->
                    ""

                Just num ->
                    String.fromFloat num

        oper =
            case model.operation of
                Clear ->
                    ""

                _ ->
                    operationAsString model.operation
    in
    num1
        ++ oper
        ++ num2
        ++ (if model.mode == Done then
                " ="

            else
                " "
           )
