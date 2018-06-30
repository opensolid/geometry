# elm-geometry [![Build Status](https://travis-ci.org/ianmackenzie/elm-geometry.svg?branch=elm-0.18)](https://travis-ci.org/ianmackenzie/elm-geometry)

_Note: Future versions of this package will be published as
[`ianmackenzie/elm-geometry`](https://github.com/ianmackenzie/elm-geometry)._

_Release notes for version 2.1 are [here](https://github.com/opensolid/geometry/releases/tag/2.1.0).
If you're upgrading from 1.x, make sure to also check out the [release notes for 2.0](https://github.com/opensolid/geometry/releases/tag/2.0.0)._

`opensolid/geometry` is an [Elm](http://elm-lang.org) package for working with
2D and 3D geometry. It provides a wide variety of geometric data types such as
points, vectors, arcs, spline curves and coordinate frames, along with functions
for transforming and combining them in many different ways. You can:

  - Rotate points around axes in 3D
  - Mirror triangles across 3D planes
  - Project 3D geometry into 2D sketch planes
  - Measure distances and angles between different objects
  - Convert objects between different coordinate systems
  - Compose complex 2D/3D transformations
  - ...and much more!

A wide variety of data types are supported:

  - `Point2d`, `Point3d`, `Vector2d`, `Vector3d`, `Direction2d`, `Direction3d`

    ![Point2d](https://opensolid.github.io/images/geometry/icons/point2d.svg)
    ![Point3d](https://opensolid.github.io/images/geometry/icons/point3d.svg)
    ![Vector2d](https://opensolid.github.io/images/geometry/icons/vector2d.svg)
    ![Vector3d](https://opensolid.github.io/images/geometry/icons/vector3d.svg)
    ![Direction2d](https://opensolid.github.io/images/geometry/icons/direction2d.svg)
    ![Direction3d](https://opensolid.github.io/images/geometry/icons/direction3d.svg)

  - `Axis2d`, `Axis3d`, `Plane3d`

    ![Axis2d](https://opensolid.github.io/images/geometry/icons/axis2d.svg)
    ![Axis3d](https://opensolid.github.io/images/geometry/icons/axis3d.svg)
    ![Plane3d](https://opensolid.github.io/images/geometry/icons/plane3d.svg)

  - `Frame2d`, `Frame3d`, `SketchPlane3d`

    ![Frame2d](https://opensolid.github.io/images/geometry/icons/frame2d.svg)
    ![Frame3d](https://opensolid.github.io/images/geometry/icons/frame3d.svg)
    ![SketchPlane3d](https://opensolid.github.io/images/geometry/icons/sketchPlane3d.svg)

  - `BoundingBox2d`, `BoundingBox3d`

    ![BoundingBox2d](https://opensolid.github.io/images/geometry/icons/boundingBox2d.svg)
    ![BoundingBox3d](https://opensolid.github.io/images/geometry/icons/boundingBox3d.svg)

  - `LineSegment2d`, `LineSegment3d`, `Triangle2d`, `Triangle3d`

    ![LineSegment2d](https://opensolid.github.io/images/geometry/icons/lineSegment2d.svg)
    ![LineSegment3d](https://opensolid.github.io/images/geometry/icons/lineSegment3d.svg)
    ![Triangle2d](https://opensolid.github.io/images/geometry/icons/triangle2d.svg)
    ![Triangle3d](https://opensolid.github.io/images/geometry/icons/triangle3d.svg)

  - `Polyline2d`, `Polyline3d`, `Polygon2d`

    ![Polyline2d](https://opensolid.github.io/images/geometry/icons/polyline2d.svg)
    ![Polyline3d](https://opensolid.github.io/images/geometry/icons/polyline3d.svg)
    ![Polygon2d](https://opensolid.github.io/images/geometry/icons/polygon2d.svg)

  - `Circle2d`, `Circle3d`, `Arc2d`, `Arc3d`

    ![Circle2d](https://opensolid.github.io/images/geometry/icons/circle2d.svg)
    ![Circle3d](https://opensolid.github.io/images/geometry/icons/circle3d.svg)
    ![Arc2d](https://opensolid.github.io/images/geometry/icons/arc2d.svg)
    ![Arc3d](https://opensolid.github.io/images/geometry/icons/arc3d.svg)

  - `Ellipse2d`, `EllipticalArc2d`

    ![Ellipse2d](https://opensolid.github.io/images/geometry/icons/ellipse2d.svg)
    ![EllipticalArc2d](https://opensolid.github.io/images/geometry/icons/ellipticalArc2d.svg)

  - `QuadraticSpline2d`, `QuadraticSpline3d`, `CubicSpline2d`, `CubicSpline3d`

    ![QuadraticSpline2d](https://opensolid.github.io/images/geometry/icons/quadraticSpline2d.svg)
    ![QuadraticSpline3d](https://opensolid.github.io/images/geometry/icons/quadraticSpline3d.svg)
    ![CubicSpline2d](https://opensolid.github.io/images/geometry/icons/cubicSpline2d.svg)
    ![CubicSpline3d](https://opensolid.github.io/images/geometry/icons/cubicSpline3d.svg)

A large range of geometric functionality is included, such as various forms of
constructors...

```elm
Point3d.fromCoordinates ( 1, 4, 5 )

Direction2d.fromAngle (degrees 30)

Point3d.midpoint p1 p2

Vector2d.withLength 3 Direction2d.y

Triangle2d.fromVertices ( p1, p2, p3 )

-- fit a plane through three points
Plane3d.throughPoints p1 p2 p3

Axis3d.through Point3d.origin Direction3d.z

Arc2d.from p1 p2 (degrees 90)

QuadraticSpline3d.with
    { startPoint = p1
    , controlPoint = p2
    , endPoint = p3
    }

CubicSpline2d.fromEndpoints
    { startPoint = p1
    , startDerivative = v1
    , endPoint = p2
    , endDerivative = v2
    }
```

...point/vector arithmetic...

```elm
Vector3d.sum v1 v2

-- the vector from the point p1 to the point p2
Vector2d.from p1 p2

Vector3d.crossProduct v1 v2

Vector2d.length vector

-- distance of a point from the origin point (0, 0)
point |> Point2d.distanceFrom Point2d.origin
```

...2D/3D transformations...

```elm
Vector2d.rotateBy (degrees 45) vector

Point2d.rotateAround Point2d.origin (degrees 45) point

Point3d.mirrorAcross Plane3d.xy point

Vector3d.projectionIn Direction3d.z vector

Triangle3d.rotateAround Axis3d.x (degrees 45) triangle

lineSegment
    |> LineSegment3d.mirrorAcross Plane3d.yz
    |> LineSegment3d.projectOnto Plane3d.xy

Plane3d.offsetBy 3 Plane3d.xy
```

...and conversions between coordinate systems:

```elm
rotatedFrame =
    Frame2d.xy |> Frame2d.rotateBy (degrees 30)

-- convert from global coordinates to local coordinates
-- (relative to the given coordinate frame)
Vector2d.relativeTo rotatedFrame vector

-- convert from local coordinates (relative to the given
-- coordinate frame) to global coordinates
Point2d.placeIn rotatedFrame point

-- convert from global 3D coordinates to local 2D
-- coordinates in the given sketch plane
point2d =
    Point3d.projectInto SketchPlane3d.yz point3d

-- convert from local 2D coordinates in the given
-- sketch plane back to global 3D coordinates
point3d =
    Point3d.on SketchPlane3d.yz point2d
```

## Installation

Assuming you have [installed Elm](https://guide.elm-lang.org/install.html) and
started a new project, use [elm-package](https://guide.elm-lang.org/install.html#elm-package)
to install `elm-geometry`, either by running

```
elm package install ianmackenzie/elm-geometry
```

in a command prompt inside your project directory or by adding

```json
"ianmackenzie/elm-geometry": "1.0.0 <= v < 2.0.0"
```

to the `dependencies` field in your project's `elm-package.json`.

## Documentation

[Full API documentation](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/1.0.0)
is available for each module. Most modules are associated with a particular data
type (for example, the [`Point3d`](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/1.0.0/Point3d)
module contains functions for creating and manipulating `Point3d` values).

## Usage details

Following the [Elm package design guidelines](http://package.elm-lang.org/help/design-guidelines#module-names-should-not-reappear-in-function-names),
most `elm-geometry` modules are designed to be imported exposing only the
corresponding types, and using the module name as a prefix for everything else:

```elm
import Point3d exposing (Point3d)
import Axis3d exposing (Axis3d)

rotatedPoint : Point3d
rotatedPoint =
    Point3d.rotateAround Axis3d.x (degrees 30) originalPoint
```

## Questions? Comments?

Please [open a new issue](https://github.com/ianmackenzie/elm-geometry/issues) if you
run into a bug, if any documentation is missing/incorrect/confusing, or if
there's a new feature that you would find useful. For general questions about
using `elm-geometry`, try:

  - Joining the **#geometry** channel on the [Elm Slack](http://elmlang.herokuapp.com/),
    or sending me (**@ianmackenzie**) a message - even if you don't have any
    particular questions right now, it would be great to know what you're hoping
    to do with the package!
  - Posting to the [Elm Discourse](https://discourse.elm-lang.org/) forums
  - Or if you happen to be in the New York area, come on out to the
    [Elm NYC meetup](https://www.meetup.com/Elm-NYC/) =)

You can also find me on Twitter ([@ianemackenzie](https://twitter.com/ianemackenzie)),
where I occasionally post `elm-geometry`-related stuff like demos or new
releases. Have fun, and don't be afraid to ask for help!
