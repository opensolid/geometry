module OpenSolid.Core.Triangle2d
  ( fromVertices
  , vertices
  , edges
  , map
  , mapTo
  , scaledAbout
  , rotatedAbout
  , translatedBy
  , mirroredAbout
  , placedOnto
  , area
  , centroid
  , boundingBox
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d


fromVertices: (Point2d, Point2d, Point2d) -> Triangle2d
fromVertices (p1, p2, p3) =
  Triangle2d p1 p2 p3


vertices: Triangle2d -> (Point2d, Point2d, Point2d)
vertices (Triangle2d p1 p2 p3) =
  (p1, p2, p3)


edges: Triangle2d -> (LineSegment2d, LineSegment2d, LineSegment2d)
edges (Triangle2d p1 p2 p3) =
  (LineSegment2d p3 p2 , LineSegment2d p1 p3, LineSegment2d p2 p1)


map: (Point2d -> Point2d) -> Triangle2d -> Triangle2d
map =
  mapTo Triangle2d


mapTo: (a -> a -> a -> b) -> (Point2d -> a) -> Triangle2d -> b
mapTo constructor map (Triangle2d p1 p2 p3) =
  constructor (map p1) (map p2) (map p3)


scaledAbout: Point2d -> Float -> Triangle2d -> Triangle2d
scaledAbout point scale =
  map (Point2d.scaledAbout point scale)


rotatedAbout: Point2d -> Float -> Triangle2d -> Triangle2d
rotatedAbout centerPoint angle =
  let
    rotatePoint = Point2d.rotatedAbout centerPoint angle
  in
    map rotatePoint


translatedBy: Vector2d -> Triangle2d -> Triangle2d
translatedBy vector =
  map (Point2d.plus vector)


mirroredAbout: Axis2d -> Triangle2d -> Triangle2d
mirroredAbout axis =
  let
    mirrorPoint = Point2d.mirroredAbout axis
  in
    map mirrorPoint


placedOnto: Plane3d -> Triangle2d -> Triangle3d
placedOnto plane =
  mapTo Triangle3d (Point2d.placedOnto plane)


area: Triangle2d -> Float
area (Triangle2d p1 p2 p3) =
  0.5 * Vector2d.cross (Point2d.vectorTo p3 p1) (Point2d.vectorTo p2 p1)


centroid: Triangle2d -> Point2d
centroid (Triangle2d p1 p2 p3) =
  let
    firstVector = Point2d.vectorTo p2 p1
    secondVector = Point2d.vectorTo p3 p1
    displacement = Vector2d.times (1.0 / 3.0) (Vector2d.plus secondVector firstVector)
  in
    Point2d.plus displacement p1


boundingBox: Triangle2d -> BoundingBox2d
boundingBox (Triangle2d (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3)) =
  let
    xMin = min x1 (min x2 x3)
    xMax = max x1 (max x2 x3)
    yMin = min y1 (min y2 y3)
    yMax = max y1 (max y2 y3)
  in
    BoundingBox2d (Interval xMin xMax) (Interval yMin yMax)
