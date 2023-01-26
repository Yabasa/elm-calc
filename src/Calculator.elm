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


operActionButtonStyle : List (Element.Attribute Msg)
operActionButtonStyle =
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
            operActionButtonStyle

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


type Action
    = Negate
    | Equal
    | Dot
    | Clear
    | ClearEntry


type IncFloat
    = Whole Int
    | WholeWithPoint Int
    | Decimal Int Int


type Msg
    = NumPressed Int
    | OperPressed Operation
    | ActionPressed Action


type Model
    = Cleared
    | InputNum1 IncFloat
    | InputOper IncFloat Operation
    | InputNum2 IncFloat Operation IncFloat
    | Done IncFloat Operation IncFloat Float



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
    Cleared



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumPressed numPressed ->
            case model of
                Cleared ->
                    InputNum1 (Whole numPressed)

                InputNum1 num1 ->
                    InputNum1 (addDigitToIncFloat num1 numPressed)

                InputOper num1 oper ->
                    InputNum2 num1 oper (Whole numPressed)

                InputNum2 num1 oper num2 ->
                    InputNum2 num1 oper (addDigitToIncFloat num2 numPressed)

                Done _ _ _ _ ->
                    model

        OperPressed operPressed ->
            case model of
                Cleared ->
                    model

                InputNum1 num1 ->
                    InputOper num1 operPressed

                InputOper num1 _ ->
                    InputOper num1 operPressed

                InputNum2 _ _ _ ->
                    model

                Done _ _ _ _ ->
                    model

        ActionPressed actionPressed ->
            case actionPressed of
                Clear ->
                    Cleared

                ClearEntry ->
                    case model of
                        Cleared ->
                            Cleared

                        InputNum1 _ ->
                            Cleared

                        InputOper num1 oper ->
                            InputOper num1 oper

                        InputNum2 num1 oper _ ->
                            InputOper num1 oper

                        Done _ _ _ _ ->
                            model

                Dot ->
                    case model of
                        Cleared ->
                            InputNum1 (dotifyIncFloat <| Whole 0)

                        InputNum1 num1 ->
                            InputNum1 (dotifyIncFloat num1)

                        InputOper num1 oper ->
                            InputNum2 num1 oper (dotifyIncFloat <| Whole 0)

                        InputNum2 num1 oper num2 ->
                            InputNum2 num1 oper (dotifyIncFloat num2)

                        Done _ _ _ _ ->
                            model

                Negate ->
                    case model of
                        Cleared ->
                            model

                        InputNum1 num1 ->
                            InputNum1 (negateNum num1)

                        InputOper _ _ ->
                            model

                        InputNum2 num1 oper num2 ->
                            InputNum2 num1 oper (negateNum num2)

                        Done _ _ _ _ ->
                            model

                Equal ->
                    case model of
                        Cleared ->
                            model

                        InputNum1 _ ->
                            model

                        InputOper _ _ ->
                            model

                        InputNum2 num1 oper num2 ->
                            let
                                num1AsFloat =
                                    incFloatAsFloat num1

                                num2AsFloat =
                                    incFloatAsFloat num2

                                result =
                                    case oper of
                                        Add ->
                                            num1AsFloat + num2AsFloat

                                        Subtract ->
                                            num1AsFloat - num2AsFloat

                                        Multiply ->
                                            num1AsFloat * num2AsFloat

                                        Divide ->
                                            num1AsFloat / num2AsFloat
                            in
                            Done num1 oper num2 result

                        Done _ _ _ _ ->
                            model


negateNum : IncFloat -> IncFloat
negateNum numToNegate =
    case numToNegate of
        Whole num ->
            Whole (negate num)

        WholeWithPoint num ->
            WholeWithPoint (negate num)

        Decimal whole decimal ->
            Decimal (negate whole) decimal


dotifyIncFloat : IncFloat -> IncFloat
dotifyIncFloat incFloat =
    case incFloat of
        Whole num ->
            WholeWithPoint num

        _ ->
            incFloat


addDigitToIncFloat : IncFloat -> Int -> IncFloat
addDigitToIncFloat currentNum newDigit =
    case currentNum of
        Whole num ->
            Whole (num * 10 + newDigit)

        WholeWithPoint num ->
            WholeWithPoint (num * 10 + newDigit)

        Decimal whole decimal ->
            Decimal whole (decimal * 10 + newDigit)


incFloatAsFloat : IncFloat -> Float
incFloatAsFloat incFloat =
    case incFloat of
        Whole num ->
            toFloat num

        WholeWithPoint num ->
            toFloat num

        Decimal whole decimal ->
            let
                decimalLength =
                    String.fromInt decimal |> String.length

                decimalValue =
                    toFloat decimal / (10 * decimalLength |> toFloat)
            in
            toFloat whole + decimalValue



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
                        [ actionButton Clear
                        , actionButton Clear
                        , actionButton Clear
                        , actionButton Clear
                        ]
                    , row [ buttonSpacing ]
                        [ actionButton Clear
                        , actionButton ClearEntry
                        , actionButton Clear
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
                        [ actionButton Negate
                        , numberButton 0
                        , actionButton Dot
                        , actionButton Equal
                        ]
                    ]
                ]


resultsArea : Model -> Element Msg
resultsArea model =
    let
        result =
            case model of
                Done _ _ _ res ->
                    String.fromFloat res

                _ ->
                    ""
    in
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
            text result
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


actionAsString : Action -> String
actionAsString oper =
    case oper of
        Negate ->
            "±"

        Equal ->
            "="

        Dot ->
            "."

        Clear ->
            "C"

        ClearEntry ->
            "CE"


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
    in
    calcButton (OperPressed oper) labelText operActionButtonStyle


actionButton : Action -> Element Msg
actionButton action =
    let
        labelText =
            el
                [ centerX
                , centerY
                , Font.hairline
                ]
                (text <| actionAsString action)

        customAttrs =
            case action of
                Equal ->
                    equalButtonStyle

                _ ->
                    operActionButtonStyle
    in
    calcButton (ActionPressed action) labelText customAttrs


incFloatAsString : IncFloat -> String
incFloatAsString incFloat =
    case incFloat of
        Whole num ->
            String.fromInt num

        WholeWithPoint num ->
            String.fromInt num

        Decimal whole decimal ->
            String.fromInt whole ++ "." ++ String.fromInt decimal


compileExpression : Model -> String
compileExpression model =
    case model of
        InputNum1 num1 ->
            incFloatAsString num1

        InputOper num1 oper ->
            incFloatAsString num1 ++ operationAsString oper

        InputNum2 num1 oper num2 ->
            incFloatAsString num1 ++ operationAsString oper ++ incFloatAsString num2

        Done num1 oper num2 _ ->
            incFloatAsString num1 ++ operationAsString oper ++ incFloatAsString num2 ++ " ="

        Cleared ->
            " "
