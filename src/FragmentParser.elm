module FragmentParser exposing (run)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser)


run : String -> Maybe (Dict String String)
run fragment =
    P.run (P.map Dict.fromList queryList) fragment |> Result.toMaybe



-- id_token=aaa&accesstoken&token_type=aaa


queryList : Parser (List ( String, String ))
queryList =
    P.sequence
        { start = ""
        , separator = "&"
        , end = ""
        , spaces = P.spaces
        , item = query
        , trailing = P.Forbidden
        }


query : Parser ( String, String )
query =
    P.succeed Tuple.pair
        |= queryString
        |. P.symbol "="
        |= queryString


queryString : Parser String
queryString =
    P.getChompedString <|
        P.succeed ()
            |. P.chompWhile
                (\c ->
                    Char.isAlphaNum c || c == '_' || c == '-' || c == '.'
                )
