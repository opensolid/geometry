module Tests.Frame2d exposing (..)

import Frame2d
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Point2d
import Test exposing (Test)


globalToGlobal : Test
globalToGlobal =
    let
        description =
            "Global -> local -> global conversion round-trips properly"

        expectation frame point =
            point
                |> Point2d.relativeTo frame
                |> Point2d.placeIn frame
                |> Expect.point2d point
    in
    Test.fuzz2 Fuzz.frame2d Fuzz.point2d description expectation


localToLocal : Test
localToLocal =
    let
        description =
            "Local -> global -> local conversion round-trips properly"

        expectation frame point =
            point
                |> Point2d.placeIn frame
                |> Point2d.relativeTo frame
                |> Expect.point2d point
    in
    Test.fuzz2 Fuzz.frame2d Fuzz.point2d description expectation
