module Tests.Sphere3d exposing (..)

import Axis3d
import BoundingBox3d
import Direction3d
import Expect exposing (FloatingPointTolerance(..))
import Future.Debug as Debug
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Plane3d
import Point3d exposing (Point3d)
import Sphere3d exposing (Sphere3d)
import Test exposing (Test)
import Triangle3d
import Vector3d


{-
   Helper methods
-}


sphereFromTuple : ( ( Float, Float, Float ), Float ) -> Sphere3d
sphereFromTuple ( centerPoint, radius ) =
    Sphere3d.withRadius radius (Point3d.fromCoordinates centerPoint)



{-
   Tests
-}


unit : Test
unit =
    Test.describe "unit"
        [ Test.test "The sphere returned by unit has radius 1" <|
            \_ ->
                Sphere3d.unit
                    |> Sphere3d.radius
                    |> Expect.approximately 1
        , Test.test "The sphere returned by unit is centered on the origin" <|
            \_ ->
                Sphere3d.unit
                    |> Sphere3d.centerPoint
                    |> Expect.point3d Point3d.origin
        ]


withRadius : Test
withRadius =
    Test.describe "withRadius"
        [ Test.fuzz2
            Fuzz.point3d
            Fuzz.scalar
            "A sphere's radius gets set correctly"
            (\centerPoint radius ->
                Sphere3d.withRadius radius centerPoint
                    |> Sphere3d.radius
                    |> Expect.approximately (abs radius)
            )
        , Test.fuzz2
            Fuzz.point3d
            Fuzz.scalar
            "A sphere's center points gets set correctly"
            (\centerPoint radius ->
                Sphere3d.withRadius radius centerPoint
                    |> Sphere3d.centerPoint
                    |> Expect.point3d centerPoint
            )
        ]


throughPointsManual : Test
throughPointsManual =
    let
        testCases =
            [ ( { c1 = ( 1, 0, 0 ), c2 = ( 0, 1, 0 ), c3 = ( -1, 0, 0 ), c4 = ( 0, 0, 1 ) }
              , ( ( 0, 0, 0 ), 1 )
              )
            , ( { c1 = ( 1, 0, 0 ), c2 = ( 0, 1, 0 ), c3 = ( -1, 0, 0 ), c4 = ( 0, 0, -1 ) }
              , ( ( 0, 0, 0 ), 1 )
              )
            , ( { c1 = ( 1, 0, 0 ), c2 = ( 0, 1, 0 ), c3 = ( -1, 0, 0 ), c4 = ( 0, 0, 0.5 ) }
              , ( ( 0, 0, -0.75 ), 1.25 )
              )
            , ( { c1 = ( 1, 0, 0 ), c2 = ( 0, 1, 0 ), c3 = ( -1, 0, 0 ), c4 = ( 0, 0, -0.5 ) }
              , ( ( 0, 0, 0.75 ), 1.25 )
              )
            ]

        inputPoints { c1, c2, c3, c4 } =
            { p1 = Point3d.fromCoordinates c1
            , p2 = Point3d.fromCoordinates c2
            , p3 = Point3d.fromCoordinates c3
            , p4 = Point3d.fromCoordinates c4
            }

        testTitle inputCoordinates expectedSphere =
            "Given " ++ Debug.toString inputCoordinates ++ " throughPoints returns " ++ Debug.toString (sphereFromTuple expectedSphere)
    in
    Test.describe "throughPoints" <|
        List.map
            (\( inputCoordinates, expectedSphere ) ->
                Test.test
                    (testTitle inputCoordinates expectedSphere)
                    (\_ ->
                        let
                            { p1, p2, p3, p4 } =
                                inputPoints inputCoordinates
                        in
                        Sphere3d.throughPoints p1 p2 p3 p4
                            |> Maybe.map (Expect.sphere3d (sphereFromTuple expectedSphere))
                            |> Maybe.withDefault (Expect.fail "throughPoints returned Nothing on valid input")
                    )
            )
            testCases


triangleArea : Point3d -> Point3d -> Point3d -> Float
triangleArea p1 p2 p3 =
    Triangle3d.fromVertices ( p1, p2, p3 ) |> Triangle3d.area


tetrahedronVolume : Point3d -> Point3d -> Point3d -> Point3d -> Float
tetrahedronVolume p1 p2 p3 p4 =
    let
        v2 =
            Vector3d.from p1 p2

        v3 =
            Vector3d.from p1 p3

        v4 =
            Vector3d.from p1 p4
    in
    abs (Vector3d.dotProduct v4 (Vector3d.crossProduct v2 v3)) / 6


validTetrahedron : Point3d -> Point3d -> Point3d -> Point3d -> Bool
validTetrahedron p1 p2 p3 p4 =
    let
        volume =
            tetrahedronVolume p1 p2 p3 p4

        limit =
            20

        isValid a b c d =
            let
                area =
                    triangleArea a b c

                isValidTriangle p q r =
                    let
                        base =
                            Point3d.distanceFrom p q

                        hIdeal =
                            base * sqrt 3 / 2

                        hMeasured =
                            2 * area / base

                        hRatio =
                            hIdeal / hMeasured
                    in
                    (1 / limit) < hRatio && hRatio < limit
            in
            isValidTriangle a b c
                && isValidTriangle b c a
                && isValidTriangle c a b
                && (let
                        yIdeal =
                            sqrt (8 * area / (3 * sqrt 3))

                        yMeasured =
                            3 * volume / area

                        ratio =
                            yIdeal / yMeasured
                    in
                    (1 / limit) < ratio && ratio < limit
                   )
    in
    isValid p1 p2 p3 p4
        && isValid p2 p3 p4 p1
        && isValid p3 p4 p1 p2
        && isValid p4 p1 p2 p3


throughPointsFuzz : Test
throughPointsFuzz =
    Test.fuzz4
        Fuzz.point3d
        Fuzz.point3d
        Fuzz.point3d
        Fuzz.point3d
        "All given points lie on the sphere constructed using `throughPoints`"
        (\p1 p2 p3 p4 ->
            if validTetrahedron p1 p2 p3 p4 then
                let
                    maybeSphere =
                        Sphere3d.throughPoints p1 p2 p3 p4

                    hasPointOnSurface point sphere =
                        Point3d.distanceFrom point (Sphere3d.centerPoint sphere)
                            |> Expect.approximately (Sphere3d.radius sphere)
                in
                case maybeSphere of
                    Just sphere ->
                        sphere
                            |> Expect.all
                                [ hasPointOnSurface p1
                                , hasPointOnSurface p2
                                , hasPointOnSurface p3
                                , hasPointOnSurface p4
                                ]

                    Nothing ->
                        Expect.fail "throughPoints returned Nothing on valid input"
            else
                Expect.pass
        )


properties : Test
properties =
    Test.fuzz Fuzz.sphere3d
        "A sphere has the correct properties (diameter, circumference, surfaceArea, volume)"
        (\sphere ->
            let
                r =
                    Sphere3d.radius sphere
            in
            sphere
                |> Expect.all
                    [ Sphere3d.diameter >> Expect.approximately (2 * r)
                    , Sphere3d.circumference >> Expect.approximately (2 * pi * r)
                    , Sphere3d.surfaceArea >> Expect.approximately (4 * pi * r * r)
                    , Sphere3d.volume >> Expect.approximately (4 / 3 * pi * r * r * r)
                    ]
        )


scaleAbout : Test
scaleAbout =
    Test.describe "scaleAbout"
        [ Test.test
            "scaleAbout correctly scales a specific sphere"
            (\_ ->
                let
                    scale =
                        0.5

                    aboutPoint =
                        Point3d.fromCoordinates ( 1, 0, 0 )

                    originalCenter =
                        Point3d.fromCoordinates ( 1, 2, 0 )

                    scaledCenter =
                        Point3d.fromCoordinates ( 1, 1, 0 )

                    originalRadius =
                        2

                    scaledRadius =
                        1
                in
                Sphere3d.withRadius originalRadius originalCenter
                    |> Sphere3d.scaleAbout aboutPoint scale
                    |> Expect.sphere3d
                        (Sphere3d.withRadius scaledRadius scaledCenter)
            )
        , Test.fuzz2 Fuzz.point3d
            Fuzz.sphere3d
            "scaling by 1 has no effect on the sphere"
            (\point sphere ->
                sphere
                    |> Sphere3d.scaleAbout point 1
                    |> Expect.sphere3d sphere
            )
        , Test.fuzz3 Fuzz.point3d
            (Fuzz.intRange 2 10)
            Fuzz.sphere3d
            "scaling and unscaling has no effect on the sphere"
            (\point scale sphere ->
                sphere
                    |> Sphere3d.scaleAbout point (toFloat scale)
                    |> Sphere3d.scaleAbout point (1 / toFloat scale)
                    |> Expect.sphere3d sphere
            )
        , Test.fuzz3 Fuzz.point3d
            Fuzz.scalar
            Fuzz.sphere3d
            "scaling changes the radius and distance to the scaling point"
            (\point scale sphere ->
                let
                    originalRadius =
                        Sphere3d.radius sphere

                    originalDistance =
                        Sphere3d.centerPoint sphere
                            |> Point3d.distanceFrom point
                in
                sphere
                    |> Sphere3d.scaleAbout point scale
                    |> Expect.all
                        [ Sphere3d.radius
                            >> Expect.approximately (originalRadius * abs scale)
                        , Sphere3d.centerPoint
                            >> Point3d.distanceFrom point
                            >> Expect.approximately (originalDistance * abs scale)
                        ]
            )
        ]


rotateAround : Test
rotateAround =
    Test.describe "rotateAround"
        [ Test.test
            "correctly rotates a specific sphere"
            (\_ ->
                let
                    axis =
                        Axis3d.z

                    -- 90 degrees
                    angle =
                        pi / 2

                    originalCenter =
                        Point3d.fromCoordinates ( 2, 0, 0 )

                    rotatedCenter =
                        Point3d.fromCoordinates ( 0, 2, 0 )

                    radius =
                        2
                in
                Sphere3d.withRadius radius originalCenter
                    |> Sphere3d.rotateAround axis angle
                    |> Expect.sphere3d
                        (Sphere3d.withRadius radius rotatedCenter)
            )
        , Test.fuzz3 Fuzz.axis3d
            Fuzz.scalar
            Fuzz.sphere3d
            "roating a sphere doesn't change its radius and its distance from the rotation axis"
            (\axis angle sphere ->
                let
                    originalDistance =
                        sphere
                            |> Sphere3d.centerPoint
                            |> Point3d.distanceFromAxis axis
                in
                sphere
                    |> Sphere3d.rotateAround axis angle
                    |> Expect.all
                        [ Sphere3d.radius
                            >> Expect.approximately (Sphere3d.radius sphere)
                        , Sphere3d.centerPoint
                            >> Point3d.distanceFromAxis axis
                            >> Expect.approximately originalDistance
                        ]
            )
        , Test.fuzz3 Fuzz.axis3d
            (Fuzz.floatRange 0 pi)
            Fuzz.sphere3d
            "rotates by the correct angle"
            (\axis angle sphere ->
                let
                    centerDistanceFromAxis =
                        Point3d.distanceFromAxis axis
                            (Sphere3d.centerPoint sphere)
                in
                if centerDistanceFromAxis > 0.001 then
                    let
                        orthogonalDirectionFromAxisTo point =
                            point
                                |> Point3d.projectOntoAxis axis
                                |> Direction3d.from point

                        maybeOriginalDirection =
                            orthogonalDirectionFromAxisTo
                                (Sphere3d.centerPoint sphere)

                        maybeRotatedDirection =
                            sphere
                                |> Sphere3d.rotateAround axis angle
                                |> Sphere3d.centerPoint
                                |> orthogonalDirectionFromAxisTo
                    in
                    case ( maybeOriginalDirection, maybeRotatedDirection ) of
                        ( Just originalDirection, Just rotatedDirection ) ->
                            let
                                angleBetweenDirections =
                                    Direction3d.angleFrom originalDirection
                                        rotatedDirection
                            in
                            angleBetweenDirections
                                |> Expect.approximately angle

                        _ ->
                            Expect.fail "Failed to compute radial directions"
                else
                    -- Don't bother checking with very small radii since
                    -- computing directions and angles is then prone to roundoff
                    Expect.pass
            )
        , Test.fuzz2 Fuzz.axis3d
            Fuzz.sphere3d
            "rotating by 2 pi doesn't change the sphere"
            (\axis sphere ->
                sphere
                    |> Sphere3d.rotateAround axis (2 * pi)
                    |> Expect.sphere3d sphere
            )
        , Test.fuzz3 Fuzz.axis3d
            Fuzz.scalar
            Fuzz.sphere3d
            "rotating and unrotating doesn't change the sphere"
            (\axis angle sphere ->
                sphere
                    |> Sphere3d.rotateAround axis angle
                    |> Sphere3d.rotateAround axis -angle
                    |> Expect.sphere3d sphere
            )
        ]


translateBy : Test
translateBy =
    Test.describe "translateBy"
        [ Test.fuzz2
            Fuzz.vector3d
            Fuzz.sphere3d
            "translating a sphere doesn't change its radius"
            (\vector sphere ->
                sphere
                    |> Sphere3d.translateBy vector
                    |> Sphere3d.radius
                    |> Expect.approximately (Sphere3d.radius sphere)
            )
        , Test.fuzz2
            Fuzz.vector3d
            Fuzz.sphere3d
            "a sphere is translated by the right amount"
            (\vector sphere ->
                sphere
                    |> Sphere3d.translateBy vector
                    |> Sphere3d.centerPoint
                    |> Vector3d.from (Sphere3d.centerPoint sphere)
                    |> Expect.vector3d vector
            )
        , Test.fuzz
            Fuzz.sphere3d
            "translating by the zero vector doesn't change the sphere"
            (\sphere ->
                sphere
                    |> Sphere3d.translateBy Vector3d.zero
                    |> Expect.sphere3d sphere
            )
        ]


mirrorAcross : Test
mirrorAcross =
    Test.describe "mirrorAcross"
        [ Test.test
            "mirrors a specific sphere correctly"
            (\_ ->
                let
                    plane =
                        Plane3d.xy

                    originalCenter =
                        Point3d.fromCoordinates ( 2, 2, 1 )

                    mirroredCenter =
                        Point3d.fromCoordinates ( 2, 2, -1 )

                    radius =
                        0.5
                in
                Sphere3d.withRadius radius originalCenter
                    |> Sphere3d.mirrorAcross plane
                    |> Expect.sphere3d
                        (Sphere3d.withRadius radius mirroredCenter)
            )
        , Test.fuzz2
            Fuzz.plane3d
            Fuzz.sphere3d
            "mirroring a sphere doesn't change its radius"
            (\plane sphere ->
                sphere
                    |> Sphere3d.mirrorAcross plane
                    |> Sphere3d.radius
                    |> Expect.approximately (Sphere3d.radius sphere)
            )
        , Test.fuzz2
            Fuzz.plane3d
            Fuzz.sphere3d
            "mirroring a sphere across a plane doesn't change the distance from the plane"
            (\plane sphere ->
                sphere
                    |> Sphere3d.mirrorAcross plane
                    |> Sphere3d.centerPoint
                    |> Point3d.signedDistanceFrom plane
                    |> Expect.approximately
                        -(Point3d.signedDistanceFrom plane
                            (Sphere3d.centerPoint sphere)
                         )
            )
        , Test.fuzz2
            Fuzz.plane3d
            Fuzz.sphere3d
            "mirroring a sphere twice yields the original sphere"
            (\plane sphere ->
                sphere
                    |> Sphere3d.mirrorAcross plane
                    |> Sphere3d.mirrorAcross plane
                    |> Expect.sphere3d sphere
            )
        ]


boundingBoxContainsCenter : Test
boundingBoxContainsCenter =
    Test.fuzz Fuzz.sphere3d
        "A sphere's bounding box contains its center point"
        (\sphere ->
            let
                boundingBox =
                    Sphere3d.boundingBox sphere

                centerPoint =
                    Sphere3d.centerPoint sphere
            in
            Expect.true
                "Circle bounding box does not contain the center point"
                (BoundingBox3d.contains centerPoint boundingBox)
        )
