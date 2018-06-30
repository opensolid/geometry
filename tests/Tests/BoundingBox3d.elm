module Tests.BoundingBox3d exposing (..)

import BoundingBox3d
import Expect
import Future.Debug as Debug
import Fuzz
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Vector3d


intersectionConsistentWithIntersects : Test
intersectionConsistentWithIntersects =
    Test.fuzz2 Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "'intersection' is consistent with 'intersects'"
        (\first second ->
            let
                intersects =
                    BoundingBox3d.intersects first second

                intersection =
                    BoundingBox3d.intersection first second
            in
            case ( intersects, intersection ) of
                ( True, Just _ ) ->
                    Expect.pass

                ( False, Nothing ) ->
                    Expect.pass

                ( True, Nothing ) ->
                    Expect.fail
                        (Debug.toString first
                            ++ " and "
                            ++ Debug.toString second
                            ++ " considered to intersect, "
                            ++ "but intersection is Nothing"
                        )

                ( False, Just intersectionBox ) ->
                    Expect.fail
                        (Debug.toString first
                            ++ " and "
                            ++ Debug.toString second
                            ++ " not considered to intersect, "
                            ++ " but have valid intersection "
                            ++ Debug.toString intersectionBox
                        )
        )


intersectionConsistentWithOverlappingBy : Test
intersectionConsistentWithOverlappingBy =
    Test.fuzz2
        Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "'intersection' is consistent with 'overlappingBy'"
        (\first second ->
            let
                overlapping =
                    BoundingBox3d.overlappingBy GT 0 first second

                intersection =
                    BoundingBox3d.intersection first second

                intersectionDimensions =
                    Maybe.map BoundingBox3d.dimensions intersection
            in
            case ( overlapping, intersectionDimensions ) of
                ( True, Just ( length, width, height ) ) ->
                    if length == 0 then
                        Expect.fail
                            (Debug.toString first
                                ++ " and "
                                ++ Debug.toString second
                                ++ " considered to strictly overlap, "
                                ++ "but intersection length is 0"
                            )
                    else if width == 0 then
                        Expect.fail
                            (Debug.toString first
                                ++ " and "
                                ++ Debug.toString second
                                ++ " considered to strictly overlap, "
                                ++ "but intersection width is 0"
                            )
                    else if height == 0 then
                        Expect.fail
                            (Debug.toString first
                                ++ " and "
                                ++ Debug.toString second
                                ++ " considered to strictly overlap, "
                                ++ "but intersection height is 0"
                            )
                    else
                        Expect.pass

                ( False, Nothing ) ->
                    Expect.pass

                ( True, Nothing ) ->
                    Expect.fail
                        (Debug.toString first
                            ++ " and "
                            ++ Debug.toString second
                            ++ " considered to strictly overlap, "
                            ++ "but intersection is Nothing"
                        )

                ( False, Just ( length, width, height ) ) ->
                    if length == 0 || height == 0 || width == 0 then
                        Expect.pass
                    else
                        Expect.fail
                            (Debug.toString first
                                ++ " and "
                                ++ Debug.toString second
                                ++ " not considered to strictly overlap, "
                                ++ "but have valid intersection "
                                ++ "with non-zero dimensions "
                                ++ Debug.toString intersectionDimensions
                            )
        )


hullContainsInputs : Test
hullContainsInputs =
    Test.fuzz2 Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "hull of two boxes contains both input boxes"
        (\first second ->
            let
                hull =
                    BoundingBox3d.hull first second

                isContained =
                    BoundingBox3d.isContainedIn hull
            in
            Expect.true "Bounding box hull does not contain both inputs"
                (isContained first && isContained second)
        )


intersectionIsValidOrNothing : Test
intersectionIsValidOrNothing =
    Test.fuzz2 Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "intersection of two boxes is either Nothing or Just a valid box"
        (\first second ->
            case BoundingBox3d.intersection first second of
                Nothing ->
                    Expect.pass

                Just result ->
                    let
                        { minX, maxX, minY, maxY, minZ, maxZ } =
                            BoundingBox3d.extrema result
                    in
                    Expect.true "expected extrema to be correctly ordered"
                        ((minX <= maxX) && (minY <= maxY) && (minZ <= maxZ))
        )


boxContainsOwnCentroid : Test
boxContainsOwnCentroid =
    Test.fuzz Fuzz.boundingBox3d
        "a bounding box contains its own centroid"
        (\box ->
            let
                centroid =
                    BoundingBox3d.centroid box
            in
            Expect.true "bounding box does not contain its own centroid"
                (BoundingBox3d.contains centroid box)
        )


overlappingByDetectsIntersection : Test
overlappingByDetectsIntersection =
    Test.fuzz2
        Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        "overlappingBy LT 0 detects non-intersecting boxes"
        (\firstBox secondBox ->
            case BoundingBox3d.intersection firstBox secondBox of
                Just intersectionBox ->
                    Expect.false "intersecting boxes should overlap by at least 0"
                        (BoundingBox3d.overlappingBy LT 0 firstBox secondBox)

                Nothing ->
                    Expect.true "non-intersecting boxes should overlap by less than 0"
                        (BoundingBox3d.overlappingBy LT 0 firstBox secondBox)
        )


overlappingBoxesCannotBySeparated : Test
overlappingBoxesCannotBySeparated =
    Test.fuzz3
        Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        Fuzz.vector3d
        "boxes overlapping by greater than a distance cannot be separated by moving that distance"
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector3d.length displacement
            in
            if BoundingBox3d.overlappingBy GT tolerance firstBox secondBox then
                BoundingBox3d.translateBy displacement firstBox
                    |> BoundingBox3d.intersects secondBox
                    |> Expect.true "displaced box should still intersect the other box"
            else
                Expect.pass
        )


separatedBoxesCannotBeMadeToOverlap : Test
separatedBoxesCannotBeMadeToOverlap =
    Test.fuzz3
        Fuzz.boundingBox3d
        Fuzz.boundingBox3d
        Fuzz.vector3d
        "boxes separated by greater than a distance cannot be made to overlap by moving that distance"
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector3d.length displacement
            in
            if BoundingBox3d.separatedBy GT tolerance firstBox secondBox then
                BoundingBox3d.translateBy displacement firstBox
                    |> BoundingBox3d.intersects secondBox
                    |> Expect.false "displaced box should still not intersect the other box"
            else
                Expect.pass
        )


separationIsCorrectForHorizontallyDisplacedBoxes : Test
separationIsCorrectForHorizontallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for horizontally displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox3d.fromExtrema
                        { minX = 0
                        , minY = 0
                        , minZ = 0
                        , maxX = 1
                        , maxY = 1
                        , maxZ = 1
                        }

                secondBox =
                    BoundingBox3d.fromExtrema
                        { minX = 2
                        , minY = 0
                        , minZ = 0
                        , maxX = 3
                        , maxY = 1
                        , maxZ = 1
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 1"
                        << BoundingBox3d.separatedBy EQ 1 secondBox
                    , Expect.true "Expected separation to be greater than 0.5"
                        << BoundingBox3d.separatedBy GT 0.5 secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox3d.separatedBy GT 0 secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox3d.separatedBy GT -1 secondBox
                    , Expect.true "Expected separation to be less than 2"
                        << BoundingBox3d.separatedBy LT 2 secondBox
                    , Expect.false "Expected separation to not be equal to 2"
                        << BoundingBox3d.separatedBy EQ 2 secondBox
                    , Expect.false "Expected separation to not be greater than 1"
                        << BoundingBox3d.separatedBy GT 1 secondBox
                    , Expect.false "Expected separation to not be less than 1"
                        << BoundingBox3d.separatedBy LT 1 secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox3d.separatedBy LT 0 secondBox
                    ]
        )


separationIsCorrectForVerticallyDisplacedBoxes : Test
separationIsCorrectForVerticallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for vertically displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox3d.fromExtrema
                        { minX = 0
                        , minY = 0
                        , minZ = 0
                        , maxX = 1
                        , maxY = 1
                        , maxZ = 1
                        }

                secondBox =
                    BoundingBox3d.fromExtrema
                        { minX = 0
                        , minY = 0
                        , minZ = 2
                        , maxX = 1
                        , maxY = 1
                        , maxZ = 3
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 1"
                        << BoundingBox3d.separatedBy EQ 1 secondBox
                    , Expect.true "Expected separation to be greater than 0.5"
                        << BoundingBox3d.separatedBy GT 0.5 secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox3d.separatedBy GT 0 secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox3d.separatedBy GT -1 secondBox
                    , Expect.true "Expected separation to be less than 2"
                        << BoundingBox3d.separatedBy LT 2 secondBox
                    , Expect.false "Expected separation to not be equal to 2"
                        << BoundingBox3d.separatedBy EQ 2 secondBox
                    , Expect.false "Expected separation to not be greater than 1"
                        << BoundingBox3d.separatedBy GT 1 secondBox
                    , Expect.false "Expected separation to not be less than 1"
                        << BoundingBox3d.separatedBy LT 1 secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox3d.separatedBy LT 0 secondBox
                    ]
        )


separationIsCorrectForDiagonallyDisplacedBoxes : Test
separationIsCorrectForDiagonallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for diagonally displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox3d.fromExtrema
                        { minX = 0
                        , minY = 0
                        , minZ = 0
                        , maxX = 1
                        , maxY = 1
                        , maxZ = 1
                        }

                secondBox =
                    BoundingBox3d.fromExtrema
                        { minX = 2
                        , minY = 3
                        , minZ = 3
                        , maxX = 4
                        , maxY = 5
                        , maxZ = 6
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 3"
                        << BoundingBox3d.separatedBy EQ 3 secondBox
                    , Expect.true "Expected separation to be greater than 2"
                        << BoundingBox3d.separatedBy GT 2 secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox3d.separatedBy GT 0 secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox3d.separatedBy GT -1 secondBox
                    , Expect.true "Expected separation to be less than 4"
                        << BoundingBox3d.separatedBy LT 4 secondBox
                    , Expect.false "Expected separation to not be equal to 4"
                        << BoundingBox3d.separatedBy EQ 4 secondBox
                    , Expect.false "Expected separation to not be greater than 3"
                        << BoundingBox3d.separatedBy GT 3 secondBox
                    , Expect.false "Expected separation to not be less than 3"
                        << BoundingBox3d.separatedBy LT 3 secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox3d.separatedBy LT 0 secondBox
                    ]
        )


containingPointsConsistentWithFromCorners : Test
containingPointsConsistentWithFromCorners =
    Test.fuzz2
        Fuzz.point3d
        Fuzz.point3d
        "'containingPoints' is consistent with 'from'"
        (\firstPoint secondPoint ->
            BoundingBox3d.containingPoints [ firstPoint, secondPoint ]
                |> Expect.equal
                    (Just (BoundingBox3d.from firstPoint secondPoint))
        )


containingPointsIsOrderIndependent : Test
containingPointsIsOrderIndependent =
    Test.fuzz (Fuzz.list Fuzz.point3d)
        "'containingPoints' does not depend on input order"
        (\points ->
            BoundingBox3d.containingPoints (List.reverse points)
                |> Expect.equal (BoundingBox3d.containingPoints points)
        )
