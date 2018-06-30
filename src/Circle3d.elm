module Circle3d
    exposing
        ( Circle3d
        , area
        , axialDirection
        , axis
        , boundingBox
        , centerPoint
        , circumference
        , diameter
        , mirrorAcross
        , on
        , placeIn
        , plane
        , projectInto
        , radius
        , relativeTo
        , rotateAround
        , scaleAbout
        , sweptAround
        , throughPoints
        , translateBy
        , translateIn
        , withRadius
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Circle3d/icon.svg" alt="Circle3d" width="160">

A `Circle3d` is defined by its center point, axial direction and radius. The
axial direction is the direction of the axis through the center of the circle
that all points on the circle are equidistant from, or equivalently the normal
direction of the plane defined by the circle. This module contains functionality
for:

  - Constructing circles around axes, on planes, or through points
  - Scaling, rotating and translating circles
  - Extracting circle properties like center point and area

@docs Circle3d


# Constructors

@docs withRadius, sweptAround, on, throughPoints


# Properties

@docs centerPoint, axialDirection, radius, diameter, axis, plane, area, circumference, boundingBox


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectInto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Circle3d =
    Types.Circle3d


{-| -}
withRadius : Float -> Direction3d -> Point3d -> Circle3d
withRadius radius_ axialDirection_ centerPoint_ =
    Types.Circle3d
        { centerPoint = centerPoint_
        , axialDirection = axialDirection_
        , radius = abs radius_
        }


{-| -}
sweptAround : Axis3d -> Point3d -> Circle3d
sweptAround axis_ point =
    let
        centerPoint_ =
            Point3d.projectOntoAxis axis_ point
    in
    withRadius (Point3d.distanceFrom centerPoint_ point)
        (Axis3d.direction axis_)
        centerPoint_


{-| -}
on : SketchPlane3d -> Circle2d -> Circle3d
on sketchPlane circle =
    withRadius (Circle2d.radius circle)
        (SketchPlane3d.normalDirection sketchPlane)
        (Point3d.on sketchPlane (Circle2d.centerPoint circle))


{-| -}
throughPoints : Point3d -> Point3d -> Point3d -> Maybe Circle3d
throughPoints p1 p2 p3 =
    Maybe.map2
        (\centerPoint_ plane_ ->
            let
                r1 =
                    Point3d.distanceFrom centerPoint_ p1

                r2 =
                    Point3d.distanceFrom centerPoint_ p2

                r3 =
                    Point3d.distanceFrom centerPoint_ p3

                r =
                    (r1 + r2 + r3) / 3
            in
            withRadius r (Plane3d.normalDirection plane_) centerPoint_
        )
        (Point3d.circumcenter p1 p2 p3)
        (Plane3d.throughPoints p1 p2 p3)


{-| -}
centerPoint : Circle3d -> Point3d
centerPoint (Types.Circle3d circle) =
    circle.centerPoint


{-| -}
axialDirection : Circle3d -> Direction3d
axialDirection (Types.Circle3d circle) =
    circle.axialDirection


{-| -}
axis : Circle3d -> Axis3d
axis (Types.Circle3d circle) =
    Axis3d.through circle.centerPoint circle.axialDirection


{-| -}
plane : Circle3d -> Plane3d
plane circle =
    Plane3d.through (centerPoint circle) (axialDirection circle)


{-| -}
radius : Circle3d -> Float
radius (Types.Circle3d properties) =
    properties.radius


{-| -}
diameter : Circle3d -> Float
diameter circle =
    2 * radius circle


{-| -}
area : Circle3d -> Float
area circle =
    let
        r =
            radius circle
    in
    pi * r * r


{-| -}
circumference : Circle3d -> Float
circumference circle =
    2 * pi * radius circle


{-| -}
scaleAbout : Point3d -> Float -> Circle3d -> Circle3d
scaleAbout point scale circle =
    withRadius (abs scale * radius circle)
        (if scale >= 0 then
            axialDirection circle
         else
            Direction3d.reverse (axialDirection circle)
        )
        (Point3d.scaleAbout point scale (centerPoint circle))


{-| -}
rotateAround : Axis3d -> Float -> Circle3d -> Circle3d
rotateAround axis_ angle =
    let
        rotatePoint =
            Point3d.rotateAround axis_ angle

        rotateDirection =
            Direction3d.rotateAround axis_ angle
    in
    \circle ->
        withRadius (radius circle)
            (rotateDirection (axialDirection circle))
            (rotatePoint (centerPoint circle))


{-| -}
translateBy : Vector3d -> Circle3d -> Circle3d
translateBy displacement circle =
    withRadius (radius circle)
        (axialDirection circle)
        (Point3d.translateBy displacement (centerPoint circle))


{-| -}
translateIn : Direction3d -> Float -> Circle3d -> Circle3d
translateIn direction distance circle =
    translateBy (Vector3d.withLength distance direction) circle


{-| -}
mirrorAcross : Plane3d -> Circle3d -> Circle3d
mirrorAcross plane_ circle =
    withRadius (radius circle)
        (Direction3d.mirrorAcross plane_ (axialDirection circle))
        (Point3d.mirrorAcross plane_ (centerPoint circle))


{-| -}
projectInto : SketchPlane3d -> Circle3d -> Types.Ellipse2d
projectInto sketchPlane circle =
    let
        projectedAxialDirection =
            axialDirection circle
                |> Direction3d.toVector
                |> Vector3d.projectInto sketchPlane

        projectedCenter =
            centerPoint circle |> Point3d.projectInto sketchPlane

        xRadius =
            radius circle
    in
    case Vector2d.direction projectedAxialDirection of
        Just yDirection ->
            let
                normalDirection =
                    SketchPlane3d.normalDirection sketchPlane

                yRatio =
                    axialDirection circle
                        |> Direction3d.componentIn normalDirection
                        |> abs

                yRadius =
                    yRatio * xRadius

                axes =
                    Frame2d.withYDirection yDirection projectedCenter
            in
            Types.Ellipse2d
                { axes = axes
                , xRadius = xRadius
                , yRadius = yRadius
                }

        Nothing ->
            Types.Ellipse2d
                { axes = Frame2d.atPoint projectedCenter
                , xRadius = xRadius
                , yRadius = xRadius
                }


{-| -}
relativeTo : Frame3d -> Circle3d -> Circle3d
relativeTo frame circle =
    withRadius (radius circle)
        (Direction3d.relativeTo frame (axialDirection circle))
        (Point3d.relativeTo frame (centerPoint circle))


{-| -}
placeIn : Frame3d -> Circle3d -> Circle3d
placeIn frame circle =
    withRadius (radius circle)
        (Direction3d.placeIn frame (axialDirection circle))
        (Point3d.placeIn frame (centerPoint circle))


{-| -}
boundingBox : Circle3d -> BoundingBox3d
boundingBox circle =
    let
        ( nx, ny, nz ) =
            Direction3d.components (axialDirection circle)

        nx2 =
            nx * nx

        ny2 =
            ny * ny

        nz2 =
            nz * nz

        r =
            radius circle

        dx =
            r * sqrt (ny2 + nz2)

        dy =
            r * sqrt (nx2 + nz2)

        dz =
            r * sqrt (nx2 + ny2)

        ( cx, cy, cz ) =
            Point3d.coordinates (centerPoint circle)
    in
    BoundingBox3d.fromExtrema
        { minX = cx - dx
        , maxX = cx + dx
        , minY = cy - dy
        , maxY = cy + dy
        , minZ = cz - dz
        , maxZ = cz + dz
        }
