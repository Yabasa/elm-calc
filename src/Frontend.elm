module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, layout, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Lamdera
import Types exposing (..)



-- STYLING


type Style
    = Windows10
    | Windows11


style : Style
style =
    Windows10


backgroundColor : Element.Attribute FrontendMsg
backgroundColor =
    case style of
        Windows10 ->
            Background.color (rgb255 230 230 230)

        Windows11 ->
            Background.color (rgb255 243 242 249)


numberButtonStyle : List (Element.Attribute FrontendMsg)
numberButtonStyle =
    case style of
        Windows10 ->
            [ Background.color (rgb255 250 250 250) ]

        Windows11 ->
            [ Background.color (rgb255 252 247 252)
            , Border.rounded 5
            ]


operActionButtonStyle : List (Element.Attribute FrontendMsg)
operActionButtonStyle =
    case style of
        Windows10 ->
            [ Background.color (rgb255 240 240 240) ]

        Windows11 ->
            [ Background.color (rgb255 252 247 250)
            , Border.rounded 5
            ]


equalButtonStyle : List (Element.Attribute FrontendMsg)
equalButtonStyle =
    case style of
        Windows10 ->
            operActionButtonStyle

        Windows11 ->
            [ Background.color (rgb255 117 87 48)
            , Border.rounded 5
            ]


app =
    Lamdera.frontend
        { init = \_ _ -> init
        , onUrlRequest = \_ -> NoOpFrontendMsg
        , onUrlChange = \_ -> NoOpFrontendMsg
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( FrontendModel, Cmd FrontendMsg )
init =
    ( Cleared
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        buttonSpacing =
            spacing 3
    in
    { title = ""
    , body =
        [ layout
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
        ]
    }


resultsArea : FrontendModel -> Element FrontendMsg
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


calcButton : FrontendMsg -> Element FrontendMsg -> List (Element.Attribute FrontendMsg) -> Element FrontendMsg
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


numberButton : Int -> Element FrontendMsg
numberButton num =
    let
        labelText =
            el [ centerX, centerY ] (text <| String.fromInt num)
    in
    calcButton (NumPressed num) labelText numberButtonStyle


operationButton : Operation -> Element FrontendMsg
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


actionButton : Action -> Element FrontendMsg
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


compileExpression : FrontendModel -> String
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
