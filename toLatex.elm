module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


--import Regex exposing (..)

import List exposing (..)
import Maybe exposing (..)
import Debug exposing (..)
import Combine exposing (..)
import Combine.Char exposing (..)
import Debug exposing (log)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    Int


model : Model
model =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            let
                test =
                    "beta"
            in
                model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ onClick Increment ] [ text ("working") ]

        --, div [] [ text (results) ]
        ]



--(?<=\\).*?(?=[^a-z A-Z])


type Expression
    = EBool Bool
    | EInt Int
    | EFloat Float
    | GHSymbols GreekHebrewLetters
    | EString String
    | EAdd Expression Expression
    | ESub Expression Expression
    | EMul Expression Expression
    | EDiv Expression Expression


type Statement
    = SExpr Expression


type CompoundStatement
    = CSimple (List Statement)
    | CS2 (List Statement) (List Statement)
    | CS3 (List Statement) (List Statement) (List Statement)


type GreekHebrewLetters
    = GH_alphs
    | GH_beta
    | GH_chi
    | GH_delta
    | GH_epsilon
    | GH_varepsilon
    | GH_eta
    | GH_gamma
    | GH_iota
    | GH_kappa
    | GH_lambda
    | GH_mu
    | GH_nu
    | GH_omega
    | GH_phi
    | GH_varphi
    | GH_pi
    | GH_psi
    | GH_rho
    | GH_sigma
    | GH_tau
    | GH_theta
    | GH_upsilon
    | GH_xi
    | GH_zeta
    | GH_Delta
    | GH_Gamma
    | GH_Lambda
    | GH_Omega
    | GH_Phi
    | GH_Pi
    | GH_Psi
    | GH_Sigma
    | GH_Theta
    | GH_Upsilon
    | GH_Xi
    | GH_aleph
    | GH_beth
    | GH_daleth
    | GH_gimel


greekAndHebrew : Parser s Expression
greekAndHebrew =
    GHSymbols
        <$> choice
                [ GH_alphs <$ string "alpha"
                , GH_beta <$ string "beta"
                , GH_delta <$ string "delta"
                , GH_epsilon <$ string "epsilon"
                , GH_varepsilon <$ string "varepsilon"
                , GH_eta <$ string "eta"
                , GH_gamma <$ string "gamma"
                , GH_iota <$ string "iota"
                , GH_kappa <$ string "kappa"
                , GH_lambda <$ string "lambda"
                , GH_mu <$ string "mu"
                , GH_nu <$ string "nu"
                , GH_omega <$ string "omega"
                , GH_phi <$ string "phi"
                , GH_varphi <$ string "varphi"
                , GH_pi <$ string "pi"
                , GH_psi <$ string "psi"
                , GH_rho <$ string "rho"
                , GH_sigma <$ string "sigma"
                , GH_tau <$ string "tau"
                , GH_theta <$ string "theta"
                , GH_upsilon <$ string "upsilon"
                , GH_xi <$ string "xi"
                , GH_zeta <$ string "zeta"
                , GH_Delta <$ string "Delta"
                , GH_Gamma <$ string "Gamma"
                , GH_Lambda <$ string "Lambda"
                , GH_Omega <$ string "Omega"
                , GH_Phi <$ string "Phi"
                , GH_Pi <$ string "Pi"
                , GH_Psi <$ string "Psi"
                , GH_Sigma <$ string "Sigma"
                , GH_Theta <$ string "Theta"
                , GH_Upsilon <$ string "Upsilon"
                , GH_Xi <$ string "Xi"
                , GH_aleph <$ string "aleph"
                , GH_beth <$ string "beth"
                , GH_daleth <$ string "daleth"
                , GH_gimel <$ string "gimel"
                ]
        <?> "not a greek letter"


simpleStmt : Parser s Expression
simpleStmt =
    lazy <|
        \() ->
            let
                stmt =
                    choice
                        [ greekAndHebrew
                        ]
            in
                --SExpr <$> sepBy (string ";" <* whitespace) stmt <* (() <$ eol <|> end)
                (whitespace <* (string "\\")) *> greekAndHebrew <* whitespace


program : Parser s (List Expression)
program =
    many1 simpleStmt


parse : String -> Result String (List Expression)
parse s =
    let
        test =
            log "going through?" 0
    in
        case Combine.parse program s of
            Ok ( _, _, es ) ->
                let
                    working =
                        log "working" 0

                    test =
                        printExpressions (es)
                in
                    Ok es

            Err ( _, stream, ms ) ->
                let
                    working =
                        log "not working" 0

                    results =
                        log (formatError ms stream) 0
                in
                    Err <| formatError ms stream


printExpressions : List Expression -> String
printExpressions ex =
    let
        test =
            scanl (log) "" (List.map toString ex)
    in
        " "


formatError : List String -> InputStream -> String
formatError ms stream =
    let
        location =
            currentLocation stream

        separator =
            "| "

        expectationSeparator =
            "\n  * "

        lineNumberOffset =
            floor (logBase 10 (toFloat location.line)) + 1

        separatorOffset =
            String.length separator

        padding =
            location.column + separatorOffset + 2
    in
        "Parse error around line:\n\n"
            ++ toString location.line
            ++ separator
            ++ location.source
            ++ "\n"
            ++ String.padLeft padding ' ' "^"
            ++ "\nI expected one of the following:\n"
            ++ expectationSeparator
            ++ String.join expectationSeparator ms



--- look into how the program does it Indentation.


test : Result String (List Expression)
test =
    parse """ \\theta \\beta """
