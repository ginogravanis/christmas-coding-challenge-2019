module Regex exposing (..)

import List
import String



{- Write a function that takes two strings as arguments, s and
   p, and returns a boolean denoting whether s matches p.

   p is a sequence of any number of the following:
       1. a-z - which stands for itself
       2. . - which matches any character
       3. * - which matches 0 or more occurrences of the
              previous single character

   Examples:

       s = "aba", p = "ab" => False

       s = "aa", p = "a*" => True

       s = "ab", p = ".*" => True

       s = "ab", p = "." => False

       s = "aab", p = "c*a*b" => True

       s = "aaa", p = "a*" => True
-}


type Token
    = This Char
    | Any
    | Group Token


isMatch : String -> String -> Bool
isMatch candidate pattern =
    let
        regex =
            lex pattern
    in
    isMatchHelp (String.toList candidate) regex


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


isMatchHelp : List Char -> List Token -> Bool
isMatchHelp chars tokens =
    case ( chars, tokens ) of
        -- The empty pattern matches the empty string.
        ( [], [] ) ->
            True

        -- Not all characters were consumed. No match.
        ( _, [] ) ->
            False

        -- Not all tokens were consumed. If the next token is a
        -- group, remove it and try again. In all other cases, no
        -- match.
        ( [], t :: ts ) ->
            case t of
                Group _ ->
                    isMatchHelp [] ts

                _ ->
                    False

        -- Default case
        ( c :: cs, t :: ts ) ->
            case t of
                Any ->
                    -- Consume first character and token.
                    isMatchHelp cs ts

                This char ->
                    -- If token and character match, consume both and
                    -- continue. Otherwise, no match.
                    if c == char then
                        isMatchHelp cs ts

                    else
                        False

                Group token ->
                    -- Either ignore group or prepend the wrapped
                    -- token and try matching it.
                    isMatchHelp chars ts
                        || isMatchHelp chars (token :: tokens)


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
