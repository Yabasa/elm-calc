module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, layout, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html.Attributes exposing (action)
import Lamdera
import MathParser as MP
import Types exposing (..)



--
-- APP
--


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

                Cleared ->
                    ( Input symbol, Cmd.none )

        ActionPressed Clear ->
            ( Cleared, Cmd.none )

        ActionPressed Backspace ->
            let
                currentInput =
                    case model of
                        Input input ->
                            input

                        Cleared ->
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

                Cleared ->
                    ( model, Cmd.none )

        ActionPressed Negate ->
            case model of
                Input currentInput ->
                    case MP.parse currentInput of
                        Ok _ ->
                            ( Input (toggleNegation currentInput), Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                Cleared ->
                    ( model, Cmd.none )

        ActionPressed SqrtWrap ->
            case model of
                Input currentInput ->
                    case MP.parse currentInput of
                        Ok _ ->
                            ( Input (toggleSqrtWrap currentInput), Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                Cleared ->
                    ( model, Cmd.none )

        ActionPressed SqrtOpen ->
            case model of
                Input currentInput ->
                    ( Input (currentInput ++ "sqrt("), Cmd.none )

                Cleared ->
                    ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


toggleNegation : String -> String
toggleNegation exprString =
    if String.left 1 exprString == "-" then
        let
            restOfInput =
                String.right (String.length exprString - 1) exprString
        in
        if String.left 1 restOfInput == "(" || isJustNumber restOfInput then
            restOfInput

        else
            exprString

    else if isWrappedInParens exprString || isJustNumber exprString then
        "-" ++ exprString

    else
        "-(" ++ exprString ++ ")"


toggleSqrtWrap : String -> String
toggleSqrtWrap exprString =
    if String.left 5 exprString == "sqrt(" then
        exprString
            |> String.right (String.length exprString - 4)

    else if String.left 1 exprString == "(" then
        "sqrt" ++ exprString

    else
        "sqrt(" ++ exprString ++ ")"


isWrappedInParens : String -> Bool
isWrappedInParens exprString =
    if String.left 1 exprString /= "(" then
        False

    else
        let
            stringLen =
                String.length exprString

            ( position, _ ) =
                String.foldl parenChecker ( 1, 1 ) (String.right (stringLen - 1) exprString)
        in
        if position == stringLen then
            True

        else
            False


parenChecker : Char -> ( Int, Int ) -> ( Int, Int )
parenChecker char ( position, parenCount ) =
    if parenCount == 0 then
        ( position, 0 )

    else if char == '(' then
        ( position + 1, parenCount + 1 )

    else if char == ')' then
        ( position + 1, parenCount - 1 )

    else
        ( position + 1, parenCount )


isJustNumber : String -> Bool
isJustNumber exprString =
    case String.toFloat exprString of
        Nothing ->
            False

        Just _ ->
            True


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
            [ actionButton SqrtOpen
            , actionButton SqrtWrap
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
            [ actionButton Negate
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
                Err _ ->
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

        Negate ->
            "-(..)"

        SqrtOpen ->
            "sqrt("

        SqrtWrap ->
            "sqrt(..)"



--
-- STYLING
--


backgroundColor : Element.Attribute FrontendMsg
backgroundColor =
    Background.color (rgb255 230 230 230)


numberButtonStyle : List (Element.Attribute FrontendMsg)
numberButtonStyle =
    [ Background.color (rgb255 250 250 250) ]


operActionButtonStyle : List (Element.Attribute FrontendMsg)
operActionButtonStyle =
    [ Background.color (rgb255 240 240 240) ]
