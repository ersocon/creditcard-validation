module Tests exposing (..)

import Test exposing (..)
import Expect
import Payment.CreditCard.Validation as CCV


all : Test
all =
    describe "Credit Card Validation Test Suite"
        [ describe "validator"
            [ test "check correct numbers as positie" <|
                \() ->
                    Expect.equal True (CCV.isValid validCreditCardNumber)
            , test "check incorrect numbers as negative" <|
                \() ->
                    Expect.equal False (CCV.isValid invalidCreditCardNumber)
            ]
        ]



-- VISA: 4716 2925 0937 5978
validCreditCardNumber: Int
validCreditCardNumber = 4716292509375978



-- 4716 2925 0937 5979
invalidCreditCardNumber: Int
invalidCreditCardNumber = 4716292509375979
