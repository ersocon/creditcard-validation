module Payment.CreditCard.Validation exposing (isValid)

import String exposing (toInt, toList, fromList)

isValid : Int -> Bool
isValid creditCardNumber =
  if creditCardNumber > 0
    && (sumDigits <| luhnSecondEach <| toListReverse creditCardNumber ) % 10 == 0 then
    True
  else
    False



luhnSecondEach : List Int -> List Int
luhnSecondEach list =
  case list of
    [] ->
      []
    [x] ->
      [x]
    x::y::_ ->
      if 2*y > 9 then
        x :: sumInt (2*y) :: luhnSecondEach (List.drop 2 list)
      else
        x :: (2*y) :: luhnSecondEach (List.drop 2 list)



toStringRepresentation: Char -> String
toStringRepresentation input =
  fromList [input]



toDigits : Int -> List Int
toDigits input =
  let
    lastDigit = input % 10
    toTransform = (input - lastDigit) // 10

    stringDigitsList = List.map toStringRepresentation (toList (toString input))
  in
    if input < 0 then
      []
    else if input < 10 then
      [input]
    else
      List.map parseInt stringDigitsList



toListReverse : Int -> List Int
toListReverse input =
  List.reverse (toDigits input)



sumDigits : List Int -> Int
sumDigits list =
  List.foldr (+) 0 list



sumInt : Int -> Int
sumInt input =
  sumDigits (toDigits input)



parseInt: String -> Int
parseInt input =
  Result.withDefault 0 (String.toInt input)
