

import Parser exposing (..)

type alias Point =
  { x : Float
  , y : Float
  }


point : Parser Point
point =
  succeed Point
    |. symbol "("
    |. spaces
    |= float
    |. spaces
    |. symbol ","
    |. spaces
    |= float
    |. spaces
    |. symbol ")"


spaces : Parser ()
spaces =
  ignore zeroOrMore (\c -> c == ' ')

{-| We start by ignoring the opening square brace and some spaces.
We only really care about the numbers, so we parse an `int` and
then use `intListHelp` to start chomping other list entries.
-}
intList : Parser (List Int)
intList =
  succeed identity
    |. symbol "["
    |. spaces
    |= andThen (\n -> intListHelp [n]) int
    |. spaces
    |. symbol "]"


{-| `intListHelp` checks if there is a `nextInt`. If so, it
continues trying to find more list items. If not, it gives
back the list of integers we have accumulated so far.
-}
intListHelp : List Int -> Parser (List Int)
intListHelp revInts =
  oneOf
    [ nextInt
        |> andThen (\n -> intListHelp (n :: revInts))
    , succeed (List.reverse revInts)
    ]

-- GOOD
nextInt : Parser Int
nextInt =
  delayedCommit spaces <|
    succeed identity
      |. symbol ","
      |. spaces
      |= int