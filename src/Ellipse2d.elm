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


{-| Construct an ellipse from its center point, X direction, and X and Y radii.
If you pass a negative radius, the absolute value will be used.

    exampleEllipse =
        Ellipse2d.with
            { centerPoint =
                Point2d.fromCoordinates ( 10, 10 )
            , xDirection =
                Direction2d.fromAngle (degrees 30)
            , xRadius = 5
            , yRadius = 3
            }

-}
with : { centerPoint : Point2d, xDirection : Direction2d, xRadius : Float, yRadius : Float } -> Ellipse2d
with properties =
    Types.Ellipse2d
        { axes =
            Frame2d.withXDirection properties.xDirection properties.centerPoint
        , xRadius = abs properties.xRadius
        , yRadius = abs properties.yRadius
        }


{-| Get the center point of an ellipse.

    Ellipse2d.centerPoint exampleEllipse
    --> Point2d.fromCoordinates ( 10, 10 )

-}
centerPoint : Ellipse2d -> Point2d
centerPoint ellipse =
    Frame2d.originPoint (axes ellipse)


{-| Get the X and Y axes of an ellipse as a `Frame2d`.

    Ellipse2d.axes exampleEllipse
    --> Frame2d.withXDirection
    -->     (Direction2d.fromAngle (degrees 30))
    -->     (Point2d.fromCoordinates ( 10, 10 ))

-}
axes : Ellipse2d -> Frame2d
axes (Types.Ellipse2d ellipse) =
    ellipse.axes


{-| Get the X axis of an ellipse.

    Ellipse2d.xAxis exampleEllipse
    --> Axis2d.through
    -->     (Point2d.fromCoordinates ( 10, 10 ))
    -->     (Direction2d.fromAngle (degrees 30))

-}
xAxis : Ellipse2d -> Axis2d
xAxis ellipse =
    Frame2d.xAxis (axes ellipse)


{-| Get the Y axis of an ellipse.

    Ellipse2d.yAxis exampleEllipse
    --> Axis2d.through
    -->     (Point2d.fromCoordinates ( 10, 10 ))
    -->     (Direction2d.fromAngle (degrees 120))

-}
yAxis : Ellipse2d -> Axis2d
yAxis ellipse =
    Frame2d.yAxis (axes ellipse)


{-| Get the radius of an ellipse along its X axis. This may be either the
minimum or maximum radius.

    Ellipse2d.xRadius exampleEllipse
    --> 5

-}
xRadius : Ellipse2d -> Float
xRadius (Types.Ellipse2d ellipse) =
    ellipse.xRadius


{-| Get the radius of an ellipse along its Y axis. This may be either the
minimum or maximum radius.

    Ellipse2d.yRadius exampleEllipse
    --> 3

-}
yRadius : Ellipse2d -> Float
yRadius (Types.Ellipse2d ellipse) =
    ellipse.yRadius


{-| Get the direction of the ellipse's X axis.

    Ellipse2d.xDirection exampleEllipse
    --> Direction2d.fromAngle (degrees 30)

-}
xDirection : Ellipse2d -> Direction2d
xDirection ellipse =
    Frame2d.xDirection (axes ellipse)


{-| Get the direction of an ellipse's Y axis.

    Ellipse2d.yDirection exampleEllipse
    --> Direction2d.fromAngle (degrees 120)

-}
yDirection : Ellipse2d -> Direction2d
yDirection ellipse =
    Frame2d.yDirection (axes ellipse)


{-| Get the area of an ellipse.

    Ellipse2d.area exampleEllipse
    --> 47.1239

-}
area : Ellipse2d -> Float
area ellipse =
    pi * xRadius ellipse * yRadius ellipse


{-| Scale an ellipse about a given point by a given scale.

    exampleEllipse
        |> Ellipse2d.scaleAbout Point2d.origin 3
    --> Ellipse2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 30, 30 )
    -->     , xDirection =
    -->         Direction2d.fromAngle (degrees 30)
    -->     , xRadius = 15
    -->     , yRadius = 9
    -->     }

-}
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


{-| Rotate an ellipse around a given point by a given angle (in radians).

    exampleEllipse
        |> Ellipse2d.rotateAround Point2d.origin
            (degrees 45)
    --> Ellipse2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 0, 14.142 )
    -->     , xDirection =
    -->         Direction2d.fromAngle (degrees 75)
    -->     , xRadius = 5
    -->     , yRadius = 3
    -->     }

-}
rotateAround : Point2d -> Float -> Ellipse2d -> Ellipse2d
rotateAround point angle =
    transformBy (Frame2d.rotateAround point angle)


{-| Translate an ellipse by a given displacement.

    exampleEllipse
        |> Ellipse2d.translateBy
            (Vector2d.fromComponents ( 5, 10 ))
    --> Ellipse2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 15, 20 )
    -->     , xDirection =
    -->         Direction2d.fromAngle (degrees 30)
    -->     , xRadius = 5
    -->     , yRadius = 3
    -->     }

-}
translateBy : Vector2d -> Ellipse2d -> Ellipse2d
translateBy displacement =
    transformBy (Frame2d.translateBy displacement)


{-| Translate an ellipse in a given direction by a given distance;

    Ellipse2d.translateIn direction distance

is equivalent to

    Ellipse2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> Ellipse2d -> Ellipse2d
translateIn direction distance ellipse =
    translateBy (Vector2d.withLength distance direction) ellipse


{-| Mirror an ellipse across a given axis.

    mirroredEllipse =
        Ellipse2d.mirrorAcross Axis2d.x exampleEllipse

    Ellipse2d.centerPoint mirroredEllipse
    --> Point2d.fromCoordinates ( 10, -10 )

    Ellipse2d.xDirection mirroredEllipse
    --> Direction2d.fromAngle (degrees -30)

    Ellipse2d.yDirection mirroredEllipse
    --> Direction2d.fromAngle (degrees -120)

Note that if the axes of the original ellipse form a [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
frame, then the axes of the mirrored ellipse will form a left-handed frame (and
vice versa).

-}
mirrorAcross : Axis2d -> Ellipse2d -> Ellipse2d
mirrorAcross axis =
    transformBy (Frame2d.mirrorAcross axis)


{-| Take an ellipse defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates (15, 5))

    Ellipse2d.relativeTo localFrame exampleEllipse
    --> Ellipse2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( -5, 5 )
    -->     , xDirection =
    -->         Direction2d.fromAngle (degrees 30)
    -->     , xRadius = 5
    -->     , yRadius = 3
    -->     }

-}
relativeTo : Frame2d -> Ellipse2d -> Ellipse2d
relativeTo frame =
    transformBy (Frame2d.relativeTo frame)


{-| Take an ellipse considered to be defined in local coordinates relative to a
given reference frame, and return that circle expressed in global coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates (15, 5))

    Ellipse2d.placeIn localFrame exampleEllipse
    --> Ellipse2d.with
    -->     { centerPoint =
    -->         Point2d.fromCoordinates ( 25, 15 )
    -->     , xDirection =
    -->         Direction2d.fromAngle (degrees 30)
    -->     , xRadius = 5
    -->     , yRadius = 3
    -->     }

-}
placeIn : Frame2d -> Ellipse2d -> Ellipse2d
placeIn frame =
    transformBy (Frame2d.placeIn frame)
