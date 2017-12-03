module Main exposing (..)

import Test exposing (..)
import Tests


suite : Test
suite =
  describe "All tests"
    [ Tests.all
    ]
