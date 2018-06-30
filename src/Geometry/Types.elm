--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Types exposing (..)


type Vector2d
    = Vector2d ( Float, Float )


type Vector3d
    = Vector3d ( Float, Float, Float )


type Direction2d
    = Direction2d ( Float, Float )


type Direction3d
    = Direction3d ( Float, Float, Float )


type Point2d
    = Point2d ( Float, Float )


type Point3d
    = Point3d ( Float, Float, Float )


type Axis2d
    = Axis2d { originPoint : Point2d, direction : Direction2d }


type Axis3d
    = Axis3d { originPoint : Point3d, direction : Direction3d }


type Plane3d
    = Plane3d { originPoint : Point3d, normalDirection : Direction3d }


type Frame2d
    = Frame2d
        { originPoint : Point2d
        , xDirection : Direction2d
        , yDirection : Direction2d
        }


type Frame3d
    = Frame3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        , zDirection : Direction3d
        }


type SketchPlane3d
    = SketchPlane3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        }


type LineSegment2d
    = LineSegment2d ( Point2d, Point2d )


type LineSegment3d
    = LineSegment3d ( Point3d, Point3d )


type Triangle2d
    = Triangle2d ( Point2d, Point2d, Point2d )


type Triangle3d
    = Triangle3d ( Point3d, Point3d, Point3d )


type BoundingBox2d
    = BoundingBox2d
        { minX : Float
        , maxX : Float
        , minY : Float
        , maxY : Float
        }


type BoundingBox3d
    = BoundingBox3d
        { minX : Float
        , maxX : Float
        , minY : Float
        , maxY : Float
        , minZ : Float
        , maxZ : Float
        }


type Rectangle2d
    = Rectangle2d
        { axes : Frame2d
        , dimensions : ( Float, Float )
        }


type Rectangle3d
    = Rectangle3d
        { axes : SketchPlane3d
        , dimensions : ( Float, Float )
        }


type Block3d
    = Block3d
        { axes : Frame3d
        , dimensions : ( Float, Float, Float )
        }


type Polyline2d
    = Polyline2d (List Point2d)


type Polyline3d
    = Polyline3d (List Point3d)


type Polygon2d
    = Polygon2d
        { outerLoop : List Point2d
        , innerLoops : List (List Point2d)
        }


type Circle2d
    = Circle2d { centerPoint : Point2d, radius : Float }


type Circle3d
    = Circle3d
        { centerPoint : Point3d
        , axialDirection : Direction3d
        , radius : Float
        }


type Ellipse2d
    = Ellipse2d
        { axes : Frame2d
        , xRadius : Float
        , yRadius : Float
        }


type Sphere3d
    = Sphere3d
        { centerPoint : Point3d
        , radius : Float
        }


type SweptAngle
    = SmallPositive
    | SmallNegative
    | LargePositive
    | LargeNegative


type Arc2d
    = Arc2d
        { startPoint : Point2d
        , xDirection : Direction2d
        , signedLength : Float
        , sweptAngle : Float
        }


type Arc3d
    = Arc3d
        { startPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        , signedLength : Float
        , sweptAngle : Float
        }


type QuadraticSpline2d
    = QuadraticSpline2d
        { startPoint : Point2d
        , controlPoint : Point2d
        , endPoint : Point2d
        }


type QuadraticSpline3d
    = QuadraticSpline3d
        { startPoint : Point3d
        , controlPoint : Point3d
        , endPoint : Point3d
        }


type CubicSpline2d
    = CubicSpline2d
        { startPoint : Point2d
        , startControlPoint : Point2d
        , endControlPoint : Point2d
        , endPoint : Point2d
        }


type CubicSpline3d
    = CubicSpline3d
        { startPoint : Point3d
        , startControlPoint : Point3d
        , endControlPoint : Point3d
        , endPoint : Point3d
        }


type EllipticalArc2d
    = EllipticalArc2d
        { ellipse : Ellipse2d
        , startAngle : Float
        , sweptAngle : Float
        }
