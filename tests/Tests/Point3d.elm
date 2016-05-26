{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Tests.Point3d exposing (suite)

import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (encode)
import ElmTest exposing (Test)
import Check exposing (Claim, claim, true, that, is, for, quickCheck)
import Check.Test exposing (evidenceToTest)
import Check.Producer as Producer
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Core.Decode as Decode
import OpenSolid.Core.Encode as Encode
import TestUtils exposing (areApproximatelyEqual)
import Producers exposing (angle, vector3d, point3d, axis3d)


rotationAboutAxisPreservesDistance : Claim
rotationAboutAxisPreservesDistance =
    let
        distancesAreEqual ( point, axis, angle ) =
            let
                distance =
                    Point3d.distanceAlong axis point

                rotatedPoint =
                    Point3d.rotateAbout axis angle point

                rotatedDistance =
                    Point3d.distanceAlong axis rotatedPoint
            in
                areApproximatelyEqual distance rotatedDistance
    in
        claim "Rotation about axis preserves distance along that axis"
            `true` distancesAreEqual
            `for` Producer.tuple3 ( point3d, axis3d, angle )


minusAndSubtractFromAreEquivalent : Claim
minusAndSubtractFromAreEquivalent =
    claim "Point3d.minus is equivalent to Vector3d.subtractFrom"
        `that` uncurry Point3d.minus
        `is` uncurry (flip Vector3d.subtractFrom)
        `for` Producer.tuple ( vector3d, point3d )


jsonRoundTrips : Claim
jsonRoundTrips =
    claim "JSON conversion round-trips properly"
        `that` (Encode.point3d >> decodeValue Decode.point3d)
        `is` Ok
        `for` point3d


suite : Test
suite =
    ElmTest.suite "Point3d tests"
        [ evidenceToTest (quickCheck rotationAboutAxisPreservesDistance)
        , evidenceToTest (quickCheck minusAndSubtractFromAreEquivalent)
        , evidenceToTest (quickCheck jsonRoundTrips)
        ]