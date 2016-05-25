import ElmTest exposing (..)
import Tests exposing (all)


main : Program Never
main =
    runSuite Tests.all
