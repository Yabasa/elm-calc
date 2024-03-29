module MathParser exposing
    ( Expr(..)
    , evaluate
    , parse
    )

import Parser exposing (..)


type Expr
    = Integer Int
    | Floating Float
    | Negation Expr
    | Add Expr Expr
    | Div Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Pow Expr Expr
    | Sqrt Expr


evaluate : Expr -> Float
evaluate expr =
    case expr of
        Integer n ->
            toFloat n

        Floating n ->
            n

        Negation a ->
            evaluate a * -1

        Add a b ->
            evaluate a + evaluate b

        Div a b ->
            evaluate a / evaluate b

        Sub a b ->
            evaluate a - evaluate b

        Mul a b ->
            evaluate a * evaluate b

        Pow a b ->
            evaluate a ^ evaluate b
        Sqrt a ->
            sqrt (evaluate a)


parse : String -> Result (List DeadEnd) Expr
parse string =
    run expression string



-- PARSER


{-| We want to handle integers, hexadecimal numbers, and floats. Octal numbers
like `0o17` and binary numbers like `0b01101100` are not allowed.

    run digits "1234"      == Ok (Integer 1234)
    run digits "-123"      == Ok (Integer -123)
    run digits "0x1b"      == Ok (Integer 27)
    run digits "3.1415"    == Ok (Floating 3.1415)
    run digits "0.1234"    == Ok (Floating 0.1234)
    run digits ".1234"     == Ok (Floating 0.1234)
    run digits "1e-42"     == Ok (Floating 1e-42)
    run digits "6.022e23"  == Ok (Floating 6.022e23)
    run digits "6.022E23"  == Ok (Floating 6.022e23)
    run digits "6.022e+23" == Ok (Floating 6.022e23)
    run digits "6.022e"    == Err ..
    run digits "6.022n"    == Err ..
    run digits "6.022.31"  == Err ..

-}
digits : Parser Expr
digits =
    number
        { int = Just Integer
        , hex = Just Integer
        , octal = Nothing
        , binary = Nothing
        , float = Just Floating
        }


term : Parser Expr
term =
    oneOf
        [ digits
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol ")"
        , succeed Sqrt
            |. keyword "sqrt"
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol ")"
        , succeed Negation
            |. symbol "-"
            |= lazy (\_ -> term)
        ]


expression : Parser Expr
expression =
    term
        |> andThen (expressionHelp [])


{-| If you want to parse operators with different precedence (like `+` and `*`)
a good strategy is to go through and create a list of all the operators. From
there, you can write separate code to sort out the grouping.
-}
expressionHelp : List ( Expr, Operator ) -> Expr -> Parser Expr
expressionHelp revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen (\( op, newExpr ) -> expressionHelp (( expr, op ) :: revOps) newExpr)
        , succeed (finalize revOps expr)
        ]


expressionHelpNonFinalized : List ( Expr, Operator ) -> Expr -> Parser ( List ( Expr, Operator ), Expr )
expressionHelpNonFinalized revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen (\( op, newExpr ) -> expressionHelpNonFinalized (( expr, op ) :: revOps) newExpr)
        , succeed ( revOps, expr )
        ]


type Operator
    = AddOp
    | DivOp
    | SubOp
    | MulOp
    | PowOp


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> AddOp) (symbol "+")
        , map (\_ -> SubOp) (symbol "-")
        , map (\_ -> MulOp) (symbol "x")
        , map (\_ -> DivOp) (symbol "/")
        , map (\_ -> PowOp) (symbol "^")
        ]


{-| We only have `+` and `*` in this parser. If we see a `MulOp` we can
immediately group those two expressions. If we see an `AddOp` we wait to group
until all the multiplies have been taken care of.

This code is kind of tricky, but it is a baseline for what you would need if
you wanted to add `/`, `-`, `==`, `&&`, etc. which bring in more complex
associativity and precedence rules.

-}
finalize : List ( Expr, Operator ) -> Expr -> Expr
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, PowOp ) :: otherRevOps ->
            finalize otherRevOps (Pow expr finalExpr)

        ( expr, MulOp ) :: otherRevOps ->
            case otherRevOps of
                ( _, PowOp ) :: _ ->
                    Mul (finalize otherRevOps expr) finalExpr

                _ ->
                    finalize otherRevOps (Mul expr finalExpr)

        ( expr, DivOp ) :: otherRevOps ->
            case otherRevOps of
                ( _, PowOp ) :: _ ->
                    Div (finalize otherRevOps expr) finalExpr

                _ ->
                    finalize otherRevOps (Div expr finalExpr)

        ( expr, AddOp ) :: otherRevOps ->
            Add (finalize otherRevOps expr) finalExpr

        ( expr, SubOp ) :: otherRevOps ->
            Sub (finalize otherRevOps expr) finalExpr
