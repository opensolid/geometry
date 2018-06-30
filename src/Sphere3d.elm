module Sphere3d
    exposing
        ( Sphere3d
        , boundingBox
        , centerPoint
        , circumference
        , contains
        , diameter
        , mirrorAcross
        , placeIn
        , projectInto
        , projectOnto
        , radius
        , relativeTo
        , rotateAround
        , scaleAbout
        , surfaceArea
        , throughPoints
        , translateBy
        , translateIn
        , unit
        , volume
        , withRadius
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Sphere3d/icon.svg" alt="Sphere3d" width="160">

A `Sphere3d` is defined by its center point and radius. This module contains
functionality for:

  - Constructing spheres through points
  - Scaling, rotating and translating spheres
  - Extracting sphere properties like center point and volume

@docs Sphere3d


# Constants

@docs unit


# Constructors

@docs withRadius, throughPoints


# Properties

@docs centerPoint, radius, diameter, volume, surfaceArea, circumference, boundingBox


# Queries

@docs contains


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectInto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types exposing (Sphere3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{--Imports for verifying the examples:

    import Geometry.Examples.Sphere3d exposing (..)
    import Geometry.Examples.Expect as Expect
    import Axis3d
    import BoundingBox3d
    import Circle2d
    import Circle3d
    import Direction3d
    import Float.Extra as Float
    import Frame3d exposing (Frame3d)
    import Plane3d
    import Point2d
    import Point3d exposing (Point3d)
    import SketchPlane3d
    import Sphere3d
    import Vector3d exposing (Vector3d)
-}


{-| -}
type alias Sphere3d =
    Types.Sphere3d


{-| The unit sphere, centered on the origin with a radius of 1.

    Sphere3d.unit
    --> Sphere3d.withRadius 1 Point3d.origin

-}
unit : Sphere3d
unit =
    withRadius 1 Point3d.origin


{-| Construct a sphere from its radius and center point:

    exampleSphere =
        Sphere3d.withRadius 3
            (Point3d.fromCoordinates ( 1, 2, 1 ))

If you pass a negative radius, the absolute value will be used.

-}
withRadius : Float -> Point3d -> Sphere3d
withRadius radius_ centerPoint_ =
    Types.Sphere3d
        { radius = abs radius_
        , centerPoint = centerPoint_
        }


{-| Attempt to construct a sphere that passes through the four given points.
Returns `Nothing` if four given points are coplanar.

    Sphere3d.throughPoints
        (Point3d.fromCoordinates ( 1, 0, 0 ))
        (Point3d.fromCoordinates ( -1, 0, 0 ))
        (Point3d.fromCoordinates ( 0, 1, 0 ))
        (Point3d.fromCoordinates ( 0, 0, 0.5 ))
    --> Just
    -->     (Sphere3d.withRadius 1.25
    -->         (Point3d.fromCoordinates ( 0, 0, -0.75 ))
    -->     )

    Sphere3d.throughPoints
        (Point3d.fromCoordinates ( 1, 0, 0 ))
        (Point3d.fromCoordinates ( -1, 0, 0 ))
        (Point3d.fromCoordinates ( 0, 1, 0 ))
        (Point3d.fromCoordinates ( 0, -1, 0 ))
    --> Nothing

-}
throughPoints : Point3d -> Point3d -> Point3d -> Point3d -> Maybe Sphere3d
throughPoints p1 p2 p3 p4 =
    Circle3d.throughPoints p1 p2 p3
        |> Maybe.andThen
            {-
               First three points define a circle.
               All points M on the normal to this circle through the circle's center are equidistant to p1, p2 and p3.
               Therefore: find this point M such that it is equidistant to p1, p2, p3 and p4,
               this will be the center of the sphere.
            -}
            (\circle ->
                let
                    normalAxis =
                        Circle3d.axis circle

                    r =
                        Circle3d.radius circle

                    x =
                        Point3d.distanceFromAxis normalAxis p4

                    y =
                        Point3d.signedDistanceAlong normalAxis p4
                in
                if y /= 0 then
                    let
                        d =
                            (r * r - x * x - y * y) / (-2 * y)
                    in
                    Just <|
                        withRadius (sqrt (r * r + d * d))
                            (Point3d.along normalAxis d)
                else
                    Nothing
            )


{-| Get the center point of a sphere.

    Sphere3d.centerPoint exampleSphere
    --> Point3d.fromCoordinates ( 1, 2, 1 )

-}
centerPoint : Sphere3d -> Point3d
centerPoint (Types.Sphere3d properties) =
    properties.centerPoint


{-| Get the radius of a sphere.

    Sphere3d.radius exampleSphere
    --> 3

-}
radius : Sphere3d -> Float
radius (Types.Sphere3d properties) =
    properties.radius


{-| Get the diameter of a sphere.

    Sphere3d.diameter exampleSphere
    --> 6

-}
diameter : Sphere3d -> Float
diameter sphere =
    2 * radius sphere


{-| Get the circumference of a sphere (the circumference of a [great circle](https://en.wikipedia.org/wiki/Great_circle)
of the sphere).

    Sphere3d.circumference exampleSphere
    --> 18.8496

-}
circumference : Sphere3d -> Float
circumference sphere =
    2 * pi * radius sphere


{-| Get the surface area of a sphere.

    Sphere3d.surfaceArea exampleSphere
    --> 113.0973

-}
surfaceArea : Sphere3d -> Float
surfaceArea sphere =
    let
        r =
            radius sphere
    in
    4 * pi * r * r


{-| Get the volume of a sphere.

    Sphere3d.volume exampleSphere
    --> 113.0973

-}
volume : Sphere3d -> Float
volume sphere =
    let
        r =
            radius sphere
    in
    4 / 3 * pi * r * r * r


{-| Scale a sphere around a given point by a given scale.

    Sphere3d.scaleAbout Point3d.origin 3 exampleSphere
    --> Sphere3d.withRadius 9
    -->     (Point3d.fromCoordinates ( 3, 6, 3 ))

-}
scaleAbout : Point3d -> Float -> Sphere3d -> Sphere3d
scaleAbout point scale sphere =
    withRadius (abs scale * radius sphere)
        (Point3d.scaleAbout point scale (centerPoint sphere))


{-| Rotate a sphere around a given axis by a given angle (in radians).

    exampleSphere
        |> Sphere3d.rotateAround Axis3d.y (degrees 90)
    --> Sphere3d.withRadius 3
    -->     (Point3d.fromCoordinates ( 1, 2, -1 ))

-}
rotateAround : Axis3d -> Float -> Sphere3d -> Sphere3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle
    in
    \sphere ->
        withRadius (radius sphere)
            (rotatePoint (centerPoint sphere))


{-| Translate a sphere by a given displacement.

    exampleSphere
        |> Sphere3d.translateBy
            (Vector3d.fromComponents ( 2, 1, 3 ))
    --> Sphere3d.withRadius 3
    -->     (Point3d.fromCoordinates ( 3, 3, 4 ))

-}
translateBy : Vector3d -> Sphere3d -> Sphere3d
translateBy displacement sphere =
    withRadius (radius sphere)
        (Point3d.translateBy displacement (centerPoint sphere))


{-| Translate a sphere in a given direction by a given distance;

    Sphere3d.translateIn direction distance

is equivalent to

    Sphere3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> Sphere3d -> Sphere3d
translateIn direction distance sphere =
    translateBy (Vector3d.withLength distance direction) sphere


{-| Mirror a sphere across a given plane.

    Sphere3d.mirrorAcross Plane3d.xy exampleSphere
    --> Sphere3d.withRadius 3
    -->     (Point3d.fromCoordinates ( 1, 2, -1 ))

-}
mirrorAcross : Plane3d -> Sphere3d -> Sphere3d
mirrorAcross plane sphere =
    withRadius (radius sphere)
        (Point3d.mirrorAcross plane (centerPoint sphere))


{-| Take a sphere defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    exampleSphere
        |> Sphere3d.relativeTo
            (Frame3d.atPoint
                (Point3d.fromCoordinates ( 1, 2, 3 ))
            )
    --> Sphere3d.withRadius 3
    -->     (Point3d.fromCoordinates ( 0, 0, -2 ))

-}
relativeTo : Frame3d -> Sphere3d -> Sphere3d
relativeTo frame sphere =
    withRadius (radius sphere)
        (Point3d.relativeTo frame (centerPoint sphere))


{-| Take a sphere considered to be defined in local coordinates relative to a
given reference frame, and return that sphere expressed in global coordinates.

    exampleSphere
        |> Sphere3d.placeIn
            (Frame3d.atPoint
                (Point3d.fromCoordinates ( 1, 2, 3 ))
            )
    --> Sphere3d.withRadius 3
    -->     (Point3d.fromCoordinates ( 2, 4, 4 ))

-}
placeIn : Frame3d -> Sphere3d -> Sphere3d
placeIn frame sphere =
    withRadius (radius sphere)
        (Point3d.placeIn frame (centerPoint sphere))


{-| Get the minimal bounding box containing a given sphere.

    Sphere3d.boundingBox exampleSphere
    --> BoundingBox3d.fromExtrema
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = -1
    -->     , maxY = 5
    -->     , minZ = -2
    -->     , maxZ = 4
    -->     }

-}
boundingBox : Sphere3d -> BoundingBox3d
boundingBox sphere =
    let
        r =
            radius sphere

        ( cx, cy, cz ) =
            Point3d.coordinates (centerPoint sphere)
    in
    BoundingBox3d.fromExtrema
        { minX = cx - r
        , maxX = cx + r
        , minY = cy - r
        , maxY = cy + r
        , minZ = cz - r
        , maxZ = cz + r
        }


{-| Check if a sphere contains a given point.

    Sphere3d.contains
        (Point3d.fromCoordinates ( 4, 2, 1 ))
        exampleSphere
    --> True

    Sphere3d.contains
        (Point3d.fromCoordinates ( 4.00001, 2, 1 ))
        exampleSphere
    --> False

-}
contains : Point3d -> Sphere3d -> Bool
contains point sphere =
    let
        r =
            radius sphere
    in
    Point3d.squaredDistanceFrom (centerPoint sphere) point <= r * r


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a sphere onto a plane.

    Sphere3d.projectOnto Plane3d.xy exampleSphere
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.fromCoordinates ( 1, 2, 0 ))

-}
projectOnto : Plane3d -> Sphere3d -> Circle3d
projectOnto plane sphere =
    Circle3d.withRadius (radius sphere)
        (Plane3d.normalDirection plane)
        (Point3d.projectOnto plane (centerPoint sphere))


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a sphere into a sketch plane.

    Sphere3d.projectInto SketchPlane3d.xy exampleSphere
    --> Circle2d.withRadius 3
    -->     (Point2d.fromCoordinates ( 1, 2 ))

-}
projectInto : SketchPlane3d -> Sphere3d -> Circle2d
projectInto sketchPlane sphere =
    Circle2d.withRadius (radius sphere)
        (Point3d.projectInto sketchPlane (centerPoint sphere))
