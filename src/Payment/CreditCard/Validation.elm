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

import String exposing (toInt, toList, fromList)


{-| Check the given credit card number for validity
-}
isValid : String -> Bool
isValid creditCardNumber =
    if String.length creditCardNumber > 0 && (sumDigits <| luhnSecondEach <| toListReverse creditCardNumber) % 10 == 0 then
        True
    else
        False


luhnSecondEach : List Int -> List Int
luhnSecondEach list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: _ ->
            if 2 * y > 9 then
                x :: sumInt (2 * y) :: luhnSecondEach (List.drop 2 list)
            else
                x :: (2 * y) :: luhnSecondEach (List.drop 2 list)


toStringRepresentation : Char -> String
toStringRepresentation input =
    fromList [ input ]


toDigits : String -> List Int
toDigits input =
    let
        lastDigit =
            input
                |> String.right 1
                |> parseInt

        stringDigitsList =
            List.map toStringRepresentation (toList input)
    in
        if parseInt input < 0 then
            []
        else
            List.map parseInt stringDigitsList


toListReverse : String -> List Int
toListReverse input =
    List.reverse (toDigits input)


sumDigits : List Int -> Int
sumDigits list =
    List.foldr (+) 0 list


sumInt : Int -> Int
sumInt input =
    sumDigits (toDigits <| toString input)



-- Simple parseInt helper (since we rely on the Int -> Bool signature for input)


parseInt : String -> Int
parseInt input =
    Result.withDefault 0 (String.toInt input)
