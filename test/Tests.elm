module Tests exposing (all)

import ElmTest exposing (..)
import Payment.CreditCard.Validation as CCV

all : Test
all =
    suite "elm-creditcard-validation"
        [ test "Valid number recognized properly" (assertEqual True (CCV.isValid validCreditCardNumber))
        , test "Invalid number is recognized properly" (assertEqual False (CCV.isValid invalidCreditCardNumber))
        , test "Zero Int" (assertEqual False (CCV.isValid 0))
        ]


-- VISA: 4716 2925 0937 5978
validCreditCardNumber: Int
validCreditCardNumber = 4716292509375978



-- 4716 2925 0937 5979
invalidCreditCardNumber: Int
invalidCreditCardNumber = 4716292509375979
