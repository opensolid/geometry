module Polygon2d.EdgeSet
    exposing
        ( Edge
        , EdgeSet
        , empty
        , insert
        , leftOf
        , remove
        )

import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)


type alias Edge =
    ( Int, LineSegment2d )


type EdgeSet
    = EdgeSet (List Edge)


empty : EdgeSet
empty =
    EdgeSet []


leftOf : Point2d -> EdgeSet -> Maybe Int
leftOf point (EdgeSet edges) =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    edges
        |> List.foldl
            (\edge current ->
                let
                    ( p1, p2 ) =
                        LineSegment2d.endpoints (Tuple.second edge)

                    ( x1, y1 ) =
                        Point2d.coordinates p1

                    ( x2, y2 ) =
                        Point2d.coordinates p2

                    dx =
                        if y1 == y2 then
                            x - max x1 x2
                        else
                            x - (x1 + ((y - y1) / (y2 - y1)) * (x2 - x1))
                in
                if dx >= 0 then
                    case current of
                        Nothing ->
                            Just ( dx, edge )

                        Just ( currentDx, currentEdge ) ->
                            if dx <= currentDx then
                                Just ( dx, edge )
                            else
                                current
                else
                    current
            )
            Nothing
        |> Maybe.map (\( dx, ( index, segment ) ) -> index)


insert : Edge -> EdgeSet -> EdgeSet
insert edge (EdgeSet edges) =
    EdgeSet (edge :: edges)


remove : Edge -> EdgeSet -> EdgeSet
remove edge (EdgeSet edges) =
    EdgeSet (List.filter ((/=) edge) edges)
