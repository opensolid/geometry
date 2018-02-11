# opensolid/geometry [![Travis build Status](https://travis-ci.org/opensolid/geometry.svg?branch=master)](https://travis-ci.org/opensolid/geometry)

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

  - `BoundingBox2d`, `BoundingBox3d`, `Interval`

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

Triangle2d.fromVertices ( p1, p2, p3 )

-- fit a plane through three points
Plane3d.throughPoints ( p1, p2, p3 )

Axis3d.with
    { originPoint = Point3d.origin
    , direction = Direction3d.z
    }

Arc2d.fromEndpoints
    { startPoint = p1
    , endPoint = p2
    , radius = 3
    , sweptAngle = Arc2d.smallPositive
    }

QuadraticSpline3d.fromControlPoints ( p1, p2, p3 )

CubicSpline2d.hermite
    ( startPoint, startDerivative )
    ( endPoint, endDerivative )
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

Support is also provided for encoding/decoding values to and from JSON.

## Installation

Assuming you have [installed Elm](https://guide.elm-lang.org/install.html) and
started a new project, use [elm-package](https://guide.elm-lang.org/install.html#elm-package)
to install `opensolid/geometry`, either by running

```
elm package install opensolid/geometry
```

in a command prompt inside your project directory or by adding

```json
"opensolid/geometry": "2.1.0 <= v < 3.0.0"
```

to the `dependencies` field in your project's `elm-package.json`.

## Documentation

[Full API documentation](http://package.elm-lang.org/packages/opensolid/geometry/2.1.0)
is available for each module. Most modules are associated with a particular data
type (for example, the [`Point3d`](http://package.elm-lang.org/packages/opensolid/geometry/2.1.0/OpenSolid-Point3d)
module contains functions for creating and manipulating `Point3d` values).

## Usage details

Following the [Elm package design guidelines](http://package.elm-lang.org/help/design-guidelines#module-names-should-not-reappear-in-function-names),
most OpenSolid modules are designed to be imported using `as` and then used as
prefixes for the functions and values that they define. Types are designed to
be imported unqualified using `exposing`:

```elm
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)

rotatedPoint : Point3d
rotatedPoint =
    Point3d.rotateAround Axis3d.x (degrees 30) originalPoint
```

## Related projects

  - [`opensolid/svg`](http://package.elm-lang.org/packages/opensolid/svg/latest)
    lets you create and manipulate SVG drawings using the data types from this
    package.
  - [`opensolid/linear-algebra-interop`](http://package.elm-lang.org/packages/opensolid/linear-algebra-interop/latest)
    provides interop support between this package and
    [`elm-community/linear-algebra`](http://package.elm-lang.org/packages/elm-community/linear-algebra/latest).

## Questions? Comments?

Please [open a new issue](https://github.com/opensolid/geometry/issues) if you
run into a bug, if any documentation is missing/incorrect/confusing, or if
there's a new feature that you would find useful. For general questions about
using OpenSolid, try:

  - Sending me (@ianmackenzie) a message on the [Elm Slack](http://elmlang.herokuapp.com/) -
    even if you don't have any particular questions right now, just come say
    hello!
  - Posting to the [Elm Discourse](https://discourse.elm-lang.org/) forums
  - Posting to the [r/elm](https://reddit.com/r/elm) subreddit
  - Or if you happen to be in the New York area, come on out to the
    [Elm NYC meetup](https://www.meetup.com/Elm-NYC/) =)

You can also find me on Twitter ([@ianemackenzie](https://twitter.com/ianemackenzie)),
where I occasionally post OpenSolid-related stuff like demos or new releases.
Have fun, and don't be afraid to ask for help!
