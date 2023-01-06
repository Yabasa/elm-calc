module Calculator exposing (main)

import Browser
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, layout, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)



--TYPES


type Operation
    = Add
    | Subtract
    | Equal
    | Clear


type Mode
    = InputNum1
    | InputNum2
    | Done


type Msg
    = NumPressed Int
    | OperPressed Operation


type alias Model =
    { num1 : List Int
    , num2 : List Int
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
    Model [] [] InputNum1 Clear Nothing



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumPressed num ->
            case model.mode of
                InputNum1 ->
                    { model | num1 = List.append model.num1 [ num ] }

                InputNum2 ->
                    { model | num2 = List.append model.num2 [ num ] }

                Done ->
                    model

        OperPressed oper ->
            case oper of
                Equal ->
                    if List.isEmpty model.num1 || List.isEmpty model.num2 then
                        model

                    else
                        let
                            num1 =
                                Maybe.withDefault 0 <| flattenNums model.num1

                            num2 =
                                Maybe.withDefault 0 <| flattenNums model.num2
                        in
                        { model
                            | mode = Done
                            , result =
                                case model.operation of
                                    Add ->
                                        Just (num1 + num2)

                                    Subtract ->
                                        Just (num1 - num2)

                                    _ ->
                                        Nothing
                        }

                Clear ->
                    { model
                        | num1 = []
                        , num2 = []
                        , mode = InputNum1
                        , operation = Clear
                        , result = Nothing
                    }

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


flattenNums : List Int -> Maybe Float
flattenNums nums =
    nums
        |> List.map String.fromInt
        |> List.foldr (++) ""
        |> String.toFloat



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        el
            [ Border.width 1
            , Border.rounded 5
            , Background.color (rgb255 230 230 230)
            , padding 20
            , centerX
            , centerY
            ]
        <|
            column
                [ spacing 20 ]
                [ resultsArea model
                , row [ spacing 50 ]
                    [ numberGrid
                    , actionButtons
                    ]
                ]


numberGrid : Element Msg
numberGrid =
    column [ spacing 10 ]
        [ row [ spacing 10 ]
            [ numberButton 1
            , numberButton 2
            , numberButton 3
            ]
        , row [ spacing 10 ]
            [ numberButton 4
            , numberButton 5
            , numberButton 6
            ]
        , row [ spacing 10 ]
            [ numberButton 7
            , numberButton 8
            , numberButton 9
            ]
        , el [ centerX ] <| numberButton 0
        ]


actionButtons : Element Msg
actionButtons =
    column [ alignTop, spacing 10 ]
        [ actionButton Add
        , actionButton Subtract
        , actionButton Equal
        , actionButton Clear
        ]


resultsArea : Model -> Element Msg
resultsArea model =
    column
        [ width fill
        , Border.width 1
        , Border.rounded 5
        ]
        [ el
            [ alignRight
            , padding 5
            , Font.size 30
            ]
          <|
            text <|
                compileExpression model
        , el
            [ Font.alignRight
            , width fill
            , height <| px 70
            , Font.size 50
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
            "-"

        Equal ->
            "="

        Clear ->
            "C"


calcButton : Msg -> String -> List (Element.Attribute Msg) -> Element Msg
calcButton msg labelText customAttrs =
    let
        defaultAttrs =
            [ Border.width 1
            , padding 5
            , width <| px 70
            , height <| px 70
            , Border.rounded 5
            ]

        finalAttrs =
            List.append defaultAttrs customAttrs
    in
    button
        finalAttrs
        { onPress = Just msg
        , label = el [ centerX, centerY, Font.size 40 ] (text labelText)
        }


numberButton : Int -> Element Msg
numberButton num =
    calcButton (NumPressed num) (String.fromInt num) [ Background.color (rgb255 250 250 250) ]


actionButton : Operation -> Element Msg
actionButton oper =
    calcButton (OperPressed oper) (operationAsString oper) [ Background.color (rgb255 240 240 240) ]


compileExpression : Model -> String
compileExpression model =
    let
        num1 =
            if List.isEmpty model.num1 then
                ""

            else
                String.fromFloat <| Maybe.withDefault 0 <| flattenNums model.num1

        num2 =
            if List.isEmpty model.num2 then
                ""

            else
                String.fromFloat <| Maybe.withDefault 0 <| flattenNums model.num2

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
