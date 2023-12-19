module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, layout, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html, input)
import Html.Attributes exposing (action)
import Lamdera
import Lamdera.Migrations exposing (ModelMigration)
import MathParser as MP
import Types exposing (..)



-- STYLING


type Style
    = Windows10
    | Windows11


style : Style
style =
    Windows10


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



--
-- UPDATE
--


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        SymbolPressed symbol ->
            case model of
                Input currentInput ->
                    ( Input <| currentInput ++ symbol, Cmd.none )

                _ ->
                    ( Input symbol, Cmd.none )

        ActionPressed Clear ->
            ( Cleared, Cmd.none )

        ActionPressed Backspace ->
            let
                currentInput =
                    case model of
                        Input input ->
                            input

                        _ ->
                            ""

                new_model =
                    if String.length currentInput < 2 then
                        Cleared

                    else if String.right 1 currentInput == ")" then
                        Input <| String.dropLeft 1 <| String.dropRight 1 currentInput

                    else
                        Input <| String.dropRight 1 currentInput
            in
            ( new_model, Cmd.none )

        ActionPressed ParenWrap ->
            case model of
                Input currentInput ->
                    ( Input <| "(" ++ currentInput ++ ")", Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )



--
-- VIEW
--


view : FrontendModel -> Browser.Document FrontendMsg
view model =
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
                    , buttonArea
                    ]
        ]
    }


buttonArea : Element FrontendMsg
buttonArea =
    let
        buttonSpacing =
            spacing 3
    in
    column [ buttonSpacing ]
        [ row [ buttonSpacing ]
            [ symbolButton ""
            , symbolButton ""
            , actionButton Clear
            , actionButton Backspace
            ]
        , row [ buttonSpacing ]
            [ symbolButton "("
            , symbolButton ")"
            , actionButton ParenWrap
            , symbolButton "^"
            ]
        , row [ buttonSpacing ]
            [ symbolButton "7"
            , symbolButton "8"
            , symbolButton "9"
            , symbolButton "/"
            ]
        , row [ buttonSpacing ]
            [ symbolButton "4"
            , symbolButton "5"
            , symbolButton "6"
            , symbolButton "x"
            ]
        , row [ buttonSpacing ]
            [ symbolButton "1"
            , symbolButton "2"
            , symbolButton "3"
            , symbolButton "-"
            ]
        , row [ buttonSpacing ]
            [ symbolButton ""
            , symbolButton "0"
            , symbolButton "."
            , symbolButton "+"
            ]
        ]


resultsArea : FrontendModel -> Element FrontendMsg
resultsArea model =
    let
        exprString =
            case model of
                Cleared ->
                    ""

                Input expr ->
                    expr

        exprParsed =
            MP.parse exprString

        result =
            case exprParsed of
                Err err ->
                    " "

                Ok expr ->
                    String.fromFloat (MP.evaluate expr)
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
                if exprString == "" then
                    " "

                else
                    exprString ++ " ="
        , el
            [ Font.alignRight
            , width fill
            , height <| px 70
            , Font.size 40
            , padding 10
            ]
          <|
            text result
        ]


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


symbolButton : String -> Element FrontendMsg
symbolButton symbol =
    let
        labelText =
            el [ centerX, centerY ] (text symbol)
    in
    calcButton (SymbolPressed symbol) labelText numberButtonStyle


actionButton : Action -> Element FrontendMsg
actionButton action =
    let
        labelText =
            el [ centerX, centerY ] (text <| actionAsString action)
    in
    calcButton (ActionPressed action) labelText numberButtonStyle


actionAsString : Action -> String
actionAsString action =
    case action of
        Clear ->
            "C"

        Backspace ->
            "Del"

        ParenWrap ->
            "(..)"


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
