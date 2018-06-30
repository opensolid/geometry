module Ellipse2d
    exposing
        ( Ellipse2d
        , area
        , axes
        , centerPoint
        , mirrorAcross
        , placeIn
        , relativeTo
        , rotateAround
        , scaleAbout
        , translateBy
        , translateIn
        , with
        , xAxis
        , xDirection
        , xRadius
        , yAxis
        , yDirection
        , yRadius
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Ellipse2d/icon.svg" alt="Ellipse2d" width="160">

An [ellipse](https://en.wikipedia.org/wiki/Ellipse) is defined by a center
point, X and Y radius, and X and Y axes (which will always be perpendicular to
each other). Ellipses are symmetric about their X and Y axes. This module
includes functionality for

  - Constructing ellipses
  - Scaling, rotating and translating ellipses
  - Converting ellipses between different coordinate systems

@docs Ellipse2d


# Constructors

@docs with


# Properties

@docs centerPoint, xAxis, yAxis, xDirection, yDirection, axes, xRadius, yRadius, area


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Ellipse2d =
    Types.Ellipse2d


{-| -}
with : { centerPoint : Point2d, xDirection : Direction2d, xRadius : Float, yRadius : Float } -> Ellipse2d
with properties =
    Types.Ellipse2d
        { axes =
            Frame2d.withXDirection properties.xDirection properties.centerPoint
        , xRadius = abs properties.xRadius
        , yRadius = abs properties.yRadius
        }


{-| -}
centerPoint : Ellipse2d -> Point2d
centerPoint ellipse =
    Frame2d.originPoint (axes ellipse)


{-| -}
axes : Ellipse2d -> Frame2d
axes (Types.Ellipse2d ellipse) =
    ellipse.axes


{-| -}
xAxis : Ellipse2d -> Axis2d
xAxis ellipse =
    Frame2d.xAxis (axes ellipse)


{-| -}
yAxis : Ellipse2d -> Axis2d
yAxis ellipse =
    Frame2d.yAxis (axes ellipse)


{-| -}
xRadius : Ellipse2d -> Float
xRadius (Types.Ellipse2d ellipse) =
    ellipse.xRadius


{-| -}
yRadius : Ellipse2d -> Float
yRadius (Types.Ellipse2d ellipse) =
    ellipse.yRadius


{-| -}
xDirection : Ellipse2d -> Direction2d
xDirection ellipse =
    Frame2d.xDirection (axes ellipse)


{-| -}
yDirection : Ellipse2d -> Direction2d
yDirection ellipse =
    Frame2d.yDirection (axes ellipse)


{-| -}
area : Ellipse2d -> Float
area ellipse =
    pi * xRadius ellipse * yRadius ellipse


{-| -}
scaleAbout : Point2d -> Float -> Ellipse2d -> Ellipse2d
scaleAbout point scale ellipse =
    let
        newCenterPoint =
            Point2d.scaleAbout point scale (centerPoint ellipse)

        newAxes =
            if scale >= 0 then
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = xDirection ellipse
                    , yDirection = yDirection ellipse
                    }
            else
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = Direction2d.reverse (xDirection ellipse)
                    , yDirection = Direction2d.reverse (yDirection ellipse)
                    }
    in
    Types.Ellipse2d
        { axes = newAxes
        , xRadius = abs (scale * xRadius ellipse)
        , yRadius = abs (scale * yRadius ellipse)
        }


transformBy : (Frame2d -> Frame2d) -> Ellipse2d -> Ellipse2d
transformBy axesTransformation (Types.Ellipse2d properties) =
    Types.Ellipse2d
        { properties | axes = axesTransformation properties.axes }


{-| -}
rotateAround : Point2d -> Float -> Ellipse2d -> Ellipse2d
rotateAround point angle =
    transformBy (Frame2d.rotateAround point angle)


{-| -}
translateBy : Vector2d -> Ellipse2d -> Ellipse2d
translateBy displacement =
    transformBy (Frame2d.translateBy displacement)


{-| -}
translateIn : Direction2d -> Float -> Ellipse2d -> Ellipse2d
translateIn direction distance ellipse =
    translateBy (Vector2d.withLength distance direction) ellipse


{-| -}
mirrorAcross : Axis2d -> Ellipse2d -> Ellipse2d
mirrorAcross axis =
    transformBy (Frame2d.mirrorAcross axis)


{-| -}
relativeTo : Frame2d -> Ellipse2d -> Ellipse2d
relativeTo frame =
    transformBy (Frame2d.relativeTo frame)


{-| -}
placeIn : Frame2d -> Ellipse2d -> Ellipse2d
placeIn frame =
    transformBy (Frame2d.placeIn frame)
