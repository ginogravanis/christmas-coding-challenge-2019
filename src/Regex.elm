module Regex exposing (..)

import List
import String


type Token
    = This Char
    | Any
    | Group Token


match : String -> String -> Bool
match pattern candidate  =
    let
        regex =
            lex pattern
    in
    matchHelp regex (String.toList candidate)


lex : String -> List Token
lex pattern =
    lexHelp (String.toList pattern) []


lexHelp : List Char -> List Token -> List Token
lexHelp chars tokens =
    case chars of
        [] ->
            List.reverse tokens

        c :: cs ->
            case c of
                '.' ->
                    lexHelp cs (Any :: tokens)

                '*' ->
                    case tokens of
                        [] ->
                            []

                        t :: ts ->
                            case t of
                                Group _ ->
                                    lexHelp cs (t :: ts)

                                _ ->
                                    lexHelp cs (Group t :: ts)

                _ ->
                    lexHelp cs (This c :: tokens)


matchHelp : List Token -> List Char -> Bool
matchHelp tokens chars =
    case ( tokens, chars ) of
        -- The empty pattern matches the empty string.
        ( [], [] ) ->
            True

        -- Not all characters were consumed. No match.
        ( [], _ ) ->
            False

        -- Not all tokens were consumed. If the next token is a
        -- group, remove it and try again. In all other cases, no
        -- match.
        ( t :: ts, [] ) ->
            case t of
                Group _ ->
                    matchHelp ts []

                _ ->
                    False

        -- Default case
        ( t :: ts, c :: cs ) ->
            case t of
                Any ->
                    -- Consume first character and token.
                    matchHelp ts cs

                This char ->
                    -- If token and character match, consume both and
                    -- continue. Otherwise, no match.
                    if c == char then
                        matchHelp ts cs

                    else
                        False

                Group token ->
                    -- Either ignore group or prepend the wrapped
                    -- token and try matching it.
                    matchHelp ts chars
                        || matchHelp (token :: tokens) chars


stripGroups : List Token -> List Token
stripGroups tokens =
    case tokens of
        [] ->
            []

        t :: ts ->
            case t of
                Group _ ->
                    stripGroups ts

                _ ->
                    tokens
