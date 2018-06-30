module Tests.BoundingBox2d exposing (..)

import BoundingBox2d
import Expect
import Future.Debug as Debug
import Fuzz
import Geometry.Fuzz as Fuzz
import Test exposing (Test)
import Vector2d


intersectionConsistentWithIntersects : Test
intersectionConsistentWithIntersects =
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "'intersection' is consistent with 'intersects'"
        (\first second ->
            let
                intersects =
                    BoundingBox2d.intersects first second

                intersection =
                    BoundingBox2d.intersection first second
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
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "'intersection' is consistent with 'overlappingBy'"
        (\first second ->
            let
                overlapping =
                    BoundingBox2d.overlappingBy GT 0 first second

                intersection =
                    BoundingBox2d.intersection first second

                intersectionDimensions =
                    Maybe.map BoundingBox2d.dimensions intersection
            in
            case ( overlapping, intersectionDimensions ) of
                ( True, Just ( width, height ) ) ->
                    if width == 0 then
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

                ( False, Just ( width, height ) ) ->
                    if height == 0 || width == 0 then
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
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "hull of two boxes contains both input boxes"
        (\first second ->
            let
                hull =
                    BoundingBox2d.hull first second

                isContained =
                    BoundingBox2d.isContainedIn hull
            in
            Expect.true "Bounding box hull does not contain both inputs"
                (isContained first && isContained second)
        )


intersectionIsValidOrNothing : Test
intersectionIsValidOrNothing =
    Test.fuzz2 Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "intersection of two boxes is either Nothing or Just a valid box"
        (\first second ->
            case BoundingBox2d.intersection first second of
                Nothing ->
                    Expect.pass

                Just result ->
                    let
                        { minX, maxX, minY, maxY } =
                            BoundingBox2d.extrema result
                    in
                    Expect.true "expected extrema to be correctly ordered"
                        ((minX <= maxX) && (minY <= maxY))
        )


boxContainsOwnCentroid : Test
boxContainsOwnCentroid =
    Test.fuzz Fuzz.boundingBox2d
        "a bounding box contains its own centroid"
        (\box ->
            let
                centroid =
                    BoundingBox2d.centroid box
            in
            Expect.true "bounding box does not contain its own centroid"
                (BoundingBox2d.contains centroid box)
        )


overlappingByDetectsIntersection : Test
overlappingByDetectsIntersection =
    Test.fuzz2
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        "overlappingBy LT 0 detects non-intersecting boxes"
        (\firstBox secondBox ->
            case BoundingBox2d.intersection firstBox secondBox of
                Just intersectionBox ->
                    Expect.false "intersecting boxes should overlap by at least 0"
                        (BoundingBox2d.overlappingBy LT 0 firstBox secondBox)

                Nothing ->
                    Expect.true "non-intersecting boxes should overlap by less than 0"
                        (BoundingBox2d.overlappingBy LT 0 firstBox secondBox)
        )


overlappingBoxesCannotBySeparated : Test
overlappingBoxesCannotBySeparated =
    Test.fuzz3
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        Fuzz.vector2d
        "boxes overlapping by greater than a distance cannot be separated by moving that distance"
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector2d.length displacement
            in
            if BoundingBox2d.overlappingBy GT tolerance firstBox secondBox then
                BoundingBox2d.translateBy displacement firstBox
                    |> BoundingBox2d.intersects secondBox
                    |> Expect.true "displaced box should still intersect the other box"
            else
                Expect.pass
        )


separatedBoxesCannotBeMadeToOverlap : Test
separatedBoxesCannotBeMadeToOverlap =
    Test.fuzz3
        Fuzz.boundingBox2d
        Fuzz.boundingBox2d
        Fuzz.vector2d
        "boxes separated by greater than a distance cannot be made to overlap by moving that distance"
        (\firstBox secondBox displacement ->
            let
                tolerance =
                    Vector2d.length displacement
            in
            if BoundingBox2d.separatedBy GT tolerance firstBox secondBox then
                BoundingBox2d.translateBy displacement firstBox
                    |> BoundingBox2d.intersects secondBox
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
                    BoundingBox2d.fromExtrema
                        { minX = 0
                        , minY = 0
                        , maxX = 1
                        , maxY = 1
                        }

                secondBox =
                    BoundingBox2d.fromExtrema
                        { minX = 2
                        , minY = 0
                        , maxX = 3
                        , maxY = 1
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 1"
                        << BoundingBox2d.separatedBy EQ 1 secondBox
                    , Expect.true "Expected separation to be greater than 0.5"
                        << BoundingBox2d.separatedBy GT 0.5 secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox2d.separatedBy GT 0 secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox2d.separatedBy GT -1 secondBox
                    , Expect.true "Expected separation to be less than 2"
                        << BoundingBox2d.separatedBy LT 2 secondBox
                    , Expect.false "Expected separation to not be equal to 2"
                        << BoundingBox2d.separatedBy EQ 2 secondBox
                    , Expect.false "Expected separation to not be greater than 1"
                        << BoundingBox2d.separatedBy GT 1 secondBox
                    , Expect.false "Expected separation to not be less than 1"
                        << BoundingBox2d.separatedBy LT 1 secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox2d.separatedBy LT 0 secondBox
                    ]
        )


separationIsCorrectForVerticallyDisplacedBoxes : Test
separationIsCorrectForVerticallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for vertically displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox2d.fromExtrema
                        { minX = 0
                        , minY = 0
                        , maxX = 1
                        , maxY = 1
                        }

                secondBox =
                    BoundingBox2d.fromExtrema
                        { minX = 0
                        , minY = 2
                        , maxX = 1
                        , maxY = 3
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 1"
                        << BoundingBox2d.separatedBy EQ 1 secondBox
                    , Expect.true "Expected separation to be greater than 0.5"
                        << BoundingBox2d.separatedBy GT 0.5 secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox2d.separatedBy GT 0 secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox2d.separatedBy GT -1 secondBox
                    , Expect.true "Expected separation to be less than 2"
                        << BoundingBox2d.separatedBy LT 2 secondBox
                    , Expect.false "Expected separation to not be equal to 2"
                        << BoundingBox2d.separatedBy EQ 2 secondBox
                    , Expect.false "Expected separation to not be greater than 1"
                        << BoundingBox2d.separatedBy GT 1 secondBox
                    , Expect.false "Expected separation to not be less than 1"
                        << BoundingBox2d.separatedBy LT 1 secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox2d.separatedBy LT 0 secondBox
                    ]
        )


separationIsCorrectForDiagonallyDisplacedBoxes : Test
separationIsCorrectForDiagonallyDisplacedBoxes =
    Test.test
        "separation is determined correctly for diagonally displaced boxes"
        (\_ ->
            let
                firstBox =
                    BoundingBox2d.fromExtrema
                        { minX = 0
                        , minY = 0
                        , maxX = 1
                        , maxY = 1
                        }

                secondBox =
                    BoundingBox2d.fromExtrema
                        { minX = 4
                        , minY = 5
                        , maxX = 5
                        , maxY = 6
                        }
            in
            firstBox
                |> Expect.all
                    [ Expect.true "Expected separation to be equal to 5"
                        << BoundingBox2d.separatedBy EQ 5 secondBox
                    , Expect.true "Expected separation to be greater than 4"
                        << BoundingBox2d.separatedBy GT 4 secondBox
                    , Expect.true "Expected separation to be greater than 0"
                        << BoundingBox2d.separatedBy GT 0 secondBox
                    , Expect.true "Expected separation to be greater than -1"
                        << BoundingBox2d.separatedBy GT -1 secondBox
                    , Expect.true "Expected separation to be less than 6"
                        << BoundingBox2d.separatedBy LT 6 secondBox
                    , Expect.false "Expected separation to not be equal to 6"
                        << BoundingBox2d.separatedBy EQ 6 secondBox
                    , Expect.false "Expected separation to not be greater than 5"
                        << BoundingBox2d.separatedBy GT 5 secondBox
                    , Expect.false "Expected separation to not be less than 5"
                        << BoundingBox2d.separatedBy LT 5 secondBox
                    , Expect.false "Expected separation to not be less than 0"
                        << BoundingBox2d.separatedBy LT 0 secondBox
                    ]
        )


containingPointsConsistentWithFromCorners : Test
containingPointsConsistentWithFromCorners =
    Test.fuzz2
        Fuzz.point2d
        Fuzz.point2d
        "'containingPoints' is consistent with from"
        (\firstPoint secondPoint ->
            BoundingBox2d.containingPoints [ firstPoint, secondPoint ]
                |> Expect.equal
                    (Just (BoundingBox2d.from firstPoint secondPoint))
        )


containingPointsIsOrderIndependent : Test
containingPointsIsOrderIndependent =
    Test.fuzz (Fuzz.list Fuzz.point2d)
        "'containingPoints' does not depend on input order"
        (\points ->
            BoundingBox2d.containingPoints (List.reverse points)
                |> Expect.equal (BoundingBox2d.containingPoints points)
        )
