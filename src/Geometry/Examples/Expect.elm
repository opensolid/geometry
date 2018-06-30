module Geometry.Examples.Expect exposing (equalWithinTolerance)

import Expect exposing (Expectation)
import Float.Extra as Float
import Regex exposing (HowMany(All), regex)
import Result
import String


{-| This is a very hacky function to compare arbitrary types and the numbers in them with a certain precision.
In order to achieve this all numbers in the string representation of the two objects are truncated and then the
resulting strings are compared.

    It should never be used.

-}
equalWithinTolerance : a -> a -> Expectation
equalWithinTolerance a b =
    let
        strA =
            toString a

        strB =
            toString b

        anyNumberRegExpr =
            regex "(-?[0-9]+(?:\\.[0-9]+)?)"

        removeAllNumbers str =
            Regex.replace All anyNumberRegExpr (always "") str

        extractAllNumbers str =
            Regex.find All anyNumberRegExpr str
                |> List.map .match

        sameNumber a b =
            let
                numA =
                    String.toFloat a

                numB =
                    String.toFloat b
            in
            Result.map2 (Float.equalWithin 1.0e-4) numA numB
                |> Result.withDefault False

        sameNumbers =
            let
                numbersA =
                    extractAllNumbers strA

                numbersB =
                    extractAllNumbers strB

                sameLength =
                    List.length numbersA == List.length numbersB
            in
            sameLength
                && (List.map2 sameNumber numbersA numbersB
                        |> List.all identity
                   )
    in
    if removeAllNumbers strA == removeAllNumbers strB && sameNumbers then
        Expect.pass
    else
        -- this custom test fails, assume that a and b are really different (even by this test's rules)
        -- use Expect.equal to get a nicely formatted output
        Expect.equal a b
