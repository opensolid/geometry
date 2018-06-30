module Rectangle2d
    exposing
        ( Rectangle2d
        , area
        , axes
        , bottomEdge
        , bottomLeftVertex
        , bottomRightVertex
        , boundingBox
        , centerPoint
        , centeredOn
        , contains
        , dimensions
        , edges
        , from
        , fromExtrema
        , fromExtremaIn
        , leftEdge
        , mirrorAcross
        , placeIn
        , relativeTo
        , rightEdge
        , rotateAround
        , scaleAbout
        , toPolygon
        , topEdge
        , topLeftVertex
        , topRightVertex
        , translateBy
        , translateIn
        , vertices
        , xAxis
        , yAxis
        )

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Vector2d exposing (Vector2d)


type alias Rectangle2d =
    Types.Rectangle2d


centeredOn : Frame2d -> ( Float, Float ) -> Rectangle2d
centeredOn givenAxes ( givenWidth, givenHeight ) =
    Types.Rectangle2d
        { axes = givenAxes
        , dimensions = ( abs givenWidth, abs givenHeight )
        }


fromExtremaIn : Frame2d -> { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Rectangle2d
fromExtremaIn localFrame { minX, maxX, minY, maxY } =
    let
        dx =
            maxX - minX

        dy =
            maxY - minY

        midX =
            minX + 0.5 * dx

        midY =
            minY + 0.5 * dy

        computedCenterPoint =
            Point2d.fromCoordinatesIn localFrame ( midX, midY )
    in
    Types.Rectangle2d
        { axes = Frame2d.moveTo computedCenterPoint localFrame
        , dimensions = ( abs dx, abs dy )
        }


from : Point2d -> Point2d -> Rectangle2d
from firstPoint secondPoint =
    let
        computedCenterPoint =
            Point2d.midpoint firstPoint secondPoint

        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
    Types.Rectangle2d
        { axes = Frame2d.atPoint computedCenterPoint
        , dimensions = ( abs (x2 - x1), abs (y2 - y1) )
        }


fromExtrema : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Rectangle2d
fromExtrema { minX, maxX, minY, maxY } =
    let
        dx =
            maxX - minX

        dy =
            maxY - minY

        midX =
            minX + 0.5 * dx

        midY =
            minY + 0.5 * dy

        computedCenterPoint =
            Point2d.fromCoordinates ( midX, midY )
    in
    Types.Rectangle2d
        { axes = Frame2d.atPoint computedCenterPoint
        , dimensions = ( abs dx, abs dy )
        }


toPolygon : Rectangle2d -> Polygon2d
toPolygon rectangle =
    let
        { bottomLeft, bottomRight, topRight, topLeft } =
            vertices rectangle
    in
    Polygon2d.singleLoop [ bottomLeft, bottomRight, topRight, topLeft ]


axes : Rectangle2d -> Frame2d
axes (Types.Rectangle2d rectangle) =
    rectangle.axes


xAxis : Rectangle2d -> Axis2d
xAxis rectangle =
    Frame2d.xAxis (axes rectangle)


yAxis : Rectangle2d -> Axis2d
yAxis rectangle =
    Frame2d.yAxis (axes rectangle)


centerPoint : Rectangle2d -> Point2d
centerPoint rectangle =
    Frame2d.originPoint (axes rectangle)


dimensions : Rectangle2d -> ( Float, Float )
dimensions (Types.Rectangle2d rectangle) =
    rectangle.dimensions


area : Rectangle2d -> Float
area rectangle =
    let
        ( width, height ) =
            dimensions rectangle
    in
    width * height


vertices : Rectangle2d -> { bottomLeft : Point2d, bottomRight : Point2d, topRight : Point2d, topLeft : Point2d }
vertices rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        halfWidth =
            width / 2

        halfHeight =
            height / 2
    in
    { bottomLeft =
        Point2d.fromCoordinatesIn localFrame ( -halfWidth, -halfHeight )
    , bottomRight =
        Point2d.fromCoordinatesIn localFrame ( halfWidth, -halfHeight )
    , topRight =
        Point2d.fromCoordinatesIn localFrame ( halfWidth, halfHeight )
    , topLeft =
        Point2d.fromCoordinatesIn localFrame ( -halfWidth, halfHeight )
    }


bottomLeftVertex : Rectangle2d -> Point2d
bottomLeftVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame ( -width / 2, -height / 2 )


bottomRightVertex : Rectangle2d -> Point2d
bottomRightVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame ( width / 2, -height / 2 )


topRightVertex : Rectangle2d -> Point2d
topRightVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame ( width / 2, height / 2 )


topLeftVertex : Rectangle2d -> Point2d
topLeftVertex rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle
    in
    Point2d.fromCoordinatesIn localFrame ( -width / 2, height / 2 )


contains : Point2d -> Rectangle2d -> Bool
contains point rectangle =
    let
        localFrame =
            axes rectangle

        ( width, height ) =
            dimensions rectangle

        ( x, y ) =
            Point2d.coordinates (Point2d.relativeTo localFrame point)
    in
    abs x <= width / 2 && abs y <= height / 2


edges : Rectangle2d -> { bottom : LineSegment2d, right : LineSegment2d, top : LineSegment2d, left : LineSegment2d }
edges rectangle =
    let
        { bottomLeft, bottomRight, topRight, topLeft } =
            vertices rectangle
    in
    { bottom = LineSegment2d.from bottomLeft bottomRight
    , right = LineSegment2d.from bottomRight topRight
    , top = LineSegment2d.from topRight topLeft
    , left = LineSegment2d.from topLeft bottomLeft
    }


bottomEdge : Rectangle2d -> LineSegment2d
bottomEdge rectangle =
    LineSegment2d.from
        (bottomLeftVertex rectangle)
        (bottomRightVertex rectangle)


rightEdge : Rectangle2d -> LineSegment2d
rightEdge rectangle =
    LineSegment2d.from
        (bottomRightVertex rectangle)
        (topRightVertex rectangle)


topEdge : Rectangle2d -> LineSegment2d
topEdge rectangle =
    LineSegment2d.from
        (topRightVertex rectangle)
        (topLeftVertex rectangle)


leftEdge : Rectangle2d -> LineSegment2d
leftEdge rectangle =
    LineSegment2d.from
        (topLeftVertex rectangle)
        (bottomLeftVertex rectangle)


scaleAbout : Point2d -> Float -> Rectangle2d -> Rectangle2d
scaleAbout point scale rectangle =
    let
        currentFrame =
            axes rectangle

        currentXDirection =
            Frame2d.xDirection currentFrame

        currentYDirection =
            Frame2d.yDirection currentFrame

        newCenterPoint =
            Point2d.scaleAbout point scale (Frame2d.originPoint currentFrame)

        newAxes =
            if scale >= 0 then
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = currentXDirection
                    , yDirection = currentYDirection
                    }
            else
                Frame2d.unsafe
                    { originPoint = newCenterPoint
                    , xDirection = Direction2d.reverse currentXDirection
                    , yDirection = Direction2d.reverse currentYDirection
                    }

        ( currentWidth, currentHeight ) =
            dimensions rectangle

        newWidth =
            abs (scale * currentWidth)

        newHeight =
            abs (scale * currentHeight)
    in
    Types.Rectangle2d
        { axes = newAxes
        , dimensions = ( newWidth, newHeight )
        }


rotateAround : Point2d -> Float -> Rectangle2d -> Rectangle2d
rotateAround point angle =
    let
        rotateFrame =
            Frame2d.rotateAround point angle
    in
    \rectangle ->
        Types.Rectangle2d
            { axes = rotateFrame (axes rectangle)
            , dimensions = dimensions rectangle
            }


translateBy : Vector2d -> Rectangle2d -> Rectangle2d
translateBy displacement rectangle =
    Types.Rectangle2d
        { axes = Frame2d.translateBy displacement (axes rectangle)
        , dimensions = dimensions rectangle
        }


{-| -}
translateIn : Direction2d -> Float -> Rectangle2d -> Rectangle2d
translateIn direction distance rectangle =
    translateBy (Vector2d.withLength distance direction) rectangle


mirrorAcross : Axis2d -> Rectangle2d -> Rectangle2d
mirrorAcross axis rectangle =
    Types.Rectangle2d
        { axes = Frame2d.mirrorAcross axis (axes rectangle)
        , dimensions = dimensions rectangle
        }


placeIn : Frame2d -> Rectangle2d -> Rectangle2d
placeIn frame rectangle =
    Types.Rectangle2d
        { axes = Frame2d.placeIn frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


relativeTo : Frame2d -> Rectangle2d -> Rectangle2d
relativeTo frame rectangle =
    Types.Rectangle2d
        { axes = Frame2d.relativeTo frame (axes rectangle)
        , dimensions = dimensions rectangle
        }


boundingBox : Rectangle2d -> BoundingBox2d
boundingBox rectangle =
    let
        { bottomLeft, bottomRight, topRight, topLeft } =
            vertices rectangle

        ( x1, y1 ) =
            Point2d.coordinates bottomLeft

        ( x2, y2 ) =
            Point2d.coordinates bottomRight

        ( x3, y3 ) =
            Point2d.coordinates topRight

        ( x4, y4 ) =
            Point2d.coordinates topLeft
    in
    BoundingBox2d.fromExtrema
        { minX = min (min x1 x2) (min x3 x4)
        , maxX = max (max x1 x2) (max x3 x4)
        , minY = min (min y1 y2) (min y3 y4)
        , maxY = max (max y1 y2) (max y3 y4)
        }
