module Bootstrap.Point3d exposing (coordinates, fromCoordinates)

import Geometry.Types exposing (..)


fromCoordinates : ( Float, Float, Float ) -> Point3d
fromCoordinates =
    Point3d


coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates_) =
    coordinates_
