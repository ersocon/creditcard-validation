module Payment.CreditCard.Validation exposing (isValid)

{-| This library can be used to validate a credit card number. It implements
the Luhn algorithm which checks for the checksum of the card. In general it
works with credit card numbers of all types (Visa, Mastercard, Maestro,
American Express, ...).

Behind the scenes it transforms the given number into a list of strings to
create single digits. This implementation may change in future. API will stay
as is.

The main use case is the validation of credit card numbers entered by users in
an input field. To avoid dependencies to parser libraries there is no
function with the signature of String -> Bool.


# Definition

@docs isValid

-}

import Regex
import String exposing (fromList, toInt, toList)


{-| Check the given credit card number for validity
-}
isValid : String -> Bool
isValid input =
    Just input
        |> Maybe.andThen isNumber
        |> Maybe.andThen checkLength
        |> Maybe.andThen checkLuhn
        |> Maybe.withDefault False


isNumber : String -> Maybe String
isNumber input =
    let
        regexRule =
            Regex.regex "^[\\d]+$"
    in
    if
        Regex.find Regex.All regexRule input
            |> List.isEmpty
    then
        Nothing
    else
        Just input


checkLength : String -> Maybe String
checkLength input =
    if input /= "" && String.length input <= 19 then
        Just input
    else
        Nothing


checkLuhn : String -> Maybe Bool
checkLuhn input =
    let
        lastDigit =
            input
                |> String.right 1
                |> parseInt

        dropLastAndReverse =
            List.reverse >> List.tail

        multiplyOdds =
            List.indexedMap
                (\i item ->
                    if i % 2 == 0 then
                        let
                            result =
                                parseInt item * 2
                        in
                        if result > 9 then
                            result - 9
                        else
                            result
                    else
                        parseInt item
                )

        checkSumMod n =
            let
                modulo =
                    n % 10
            in
            if modulo > 0 then
                10 - modulo == lastDigit
            else
                modulo == lastDigit
    in
    input
        |> String.split ""
        |> dropLastAndReverse
        |> Maybe.map multiplyOdds
        |> Maybe.map List.sum
        |> Maybe.map checkSumMod



-- Simple parseInt helper (since we rely on the Int -> Bool signature for input)


parseInt : String -> Int
parseInt input =
    Result.withDefault 0 (String.toInt input)
