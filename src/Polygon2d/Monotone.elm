module Polygon2d.Monotone
    exposing
        ( faces
        , init
        , monotonePolygons
        , testPolygon
        , triangulation
        )

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Future.Tuple as Tuple
import Geometry.Types as Types exposing (Polygon2d(..))
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d.EdgeSet as EdgeSet exposing (EdgeSet)
import Set exposing (Set)
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)


type Kind
    = Start
    | End
    | Left
    | Right
    | Split
    | Merge


type alias Vertex =
    { position : Point2d
    , kind : Kind
    }


comparePoints : Point2d -> Point2d -> Order
comparePoints p1 p2 =
    let
        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2
    in
    if y1 < y2 then
        LT
    else if y1 > y2 then
        GT
    else
        compare x2 x1


leftTurn : Point2d -> Point2d -> Point2d -> Bool
leftTurn p1 p2 p3 =
    let
        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3
    in
    (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2) > 0


kind : Point2d -> Point2d -> Point2d -> Kind
kind previous current next =
    let
        compareToPrevious =
            comparePoints current previous

        compareToNext =
            comparePoints current next
    in
    if compareToPrevious == GT && compareToNext == GT then
        if leftTurn previous current next then
            Start
        else
            Split
    else if compareToPrevious == LT && compareToNext == LT then
        if leftTurn previous current next then
            End
        else
            Merge
    else if compareToPrevious == GT then
        Right
    else
        Left


toVertices : List Point2d -> List Vertex
toVertices points =
    case points of
        [] ->
            []

        [ singlePoint ] ->
            -- TODO use as combined split/merge vertex
            []

        [ firstPoint, secondPoint ] ->
            if comparePoints firstPoint secondPoint == GT then
                [ { position = firstPoint, kind = Split }
                , { position = secondPoint, kind = Merge }
                ]
            else
                [ { position = firstPoint, kind = Merge }
                , { position = secondPoint, kind = Split }
                ]

        firstPoint :: secondPoint :: thirdPoint :: rest ->
            let
                lastPoint =
                    List.foldl always thirdPoint rest

                collect previousPoint currentPoint remainingPoints accumulated =
                    case remainingPoints of
                        [] ->
                            let
                                lastVertex =
                                    { position = currentPoint
                                    , kind =
                                        kind
                                            previousPoint
                                            currentPoint
                                            firstPoint
                                    }
                            in
                            List.reverse (lastVertex :: accumulated)

                        nextPoint :: followingPoints ->
                            let
                                newVertex =
                                    { position = currentPoint
                                    , kind =
                                        kind
                                            previousPoint
                                            currentPoint
                                            nextPoint
                                    }
                            in
                            collect
                                currentPoint
                                nextPoint
                                followingPoints
                                (newVertex :: accumulated)
            in
            collect lastPoint firstPoint (secondPoint :: thirdPoint :: rest) []


type alias Edge =
    { startVertexIndex : Int
    , endVertexIndex : Int
    , nextEdgeIndex : Int
    , previousEdgeIndex : Int
    }


type alias Loops =
    { vertices : List Vertex
    , edges : Array Edge
    }


removeDuplicates : List Point2d -> List Point2d
removeDuplicates points =
    case points of
        [] ->
            []

        firstPoint :: rest ->
            let
                -- Strip out adjacent duplicates
                accumulatedPoints =
                    accumulateDistinctPoints firstPoint rest []
            in
            case accumulatedPoints of
                lastPoint :: otherPoints ->
                    if lastPoint == firstPoint then
                        -- Drop the last point since it's equal to the
                        -- first
                        firstPoint :: List.reverse otherPoints
                    else
                        -- Keep all points
                        firstPoint :: List.reverse accumulatedPoints

                [] ->
                    -- Just have the first point
                    [ firstPoint ]


accumulateDistinctPoints : Point2d -> List Point2d -> List Point2d -> List Point2d
accumulateDistinctPoints previousPoint points accumulatedPoints =
    case points of
        [] ->
            accumulatedPoints

        point :: rest ->
            let
                updatedPoints =
                    if point == previousPoint then
                        accumulatedPoints
                    else
                        point :: accumulatedPoints
            in
            accumulateDistinctPoints point rest updatedPoints


init : Polygon2d -> Loops
init (Polygon2d { outerLoop, innerLoops }) =
    let
        allLoops =
            (outerLoop :: innerLoops)
                |> List.map (\loop -> removeDuplicates loop)

        vertices =
            allLoops
                |> List.map toVertices
                |> List.concat

        edges =
            List.foldl
                (\loop ( offset, accumulated ) ->
                    let
                        length =
                            List.length loop

                        newEdges =
                            Array.initialize length
                                (\index ->
                                    if index == 0 then
                                        { startVertexIndex =
                                            offset
                                        , endVertexIndex =
                                            offset + 1
                                        , nextEdgeIndex =
                                            offset + 1
                                        , previousEdgeIndex =
                                            offset + length - 1
                                        }
                                    else if index == length - 1 then
                                        { startVertexIndex =
                                            offset + index
                                        , endVertexIndex =
                                            offset
                                        , nextEdgeIndex =
                                            offset
                                        , previousEdgeIndex =
                                            offset + index - 1
                                        }
                                    else
                                        { startVertexIndex =
                                            offset + index
                                        , endVertexIndex =
                                            offset + index + 1
                                        , nextEdgeIndex =
                                            offset + index + 1
                                        , previousEdgeIndex =
                                            offset + index - 1
                                        }
                                )
                    in
                    ( offset + length, Array.append accumulated newEdges )
                )
                ( 0, Array.empty )
                allLoops
                |> Tuple.second
    in
    { vertices = vertices
    , edges = edges
    }


type alias HelperVertex =
    { index : Int
    , outgoingEdgeIndex : Int
    , isMerge : Bool
    }


type alias State =
    { edgeSet : EdgeSet
    , helpers : Dict Int HelperVertex -- helper vertex by edge index
    , vertices : Array Vertex
    , edges : Array Edge
    }


getVertex : Int -> State -> Maybe Vertex
getVertex index state =
    Array.get index state.vertices


getEdge : Int -> State -> Maybe Edge
getEdge index state =
    Array.get index state.edges


error : a -> a
error defaultValue =
    let
        _ =
            Debug.log "ERROR: please file a bug report at https://github.com/ianmackenzie/elm-geometry/issues"
                "Internal error in Polygon2d.Monotone module"
    in
    defaultValue


defaultTo : a -> Maybe a -> a
defaultTo defaultValue maybeValue =
    case maybeValue of
        Just actualValue ->
            actualValue

        Nothing ->
            error defaultValue


processLeftEdge : (EdgeSet.Edge -> EdgeSet -> EdgeSet) -> Int -> State -> State
processLeftEdge insertOrRemove edgeIndex state =
    getEdge edgeIndex state
        |> Maybe.andThen
            (\edge ->
                Maybe.map2
                    (\startVertex endVertex ->
                        let
                            lineSegment =
                                LineSegment2d.fromEndpoints
                                    ( startVertex.position
                                    , endVertex.position
                                    )
                        in
                        { state
                            | edgeSet =
                                state.edgeSet
                                    |> insertOrRemove ( edgeIndex, lineSegment )
                        }
                    )
                    (getVertex edge.startVertexIndex state)
                    (getVertex edge.endVertexIndex state)
            )
        |> defaultTo state


insertLeftEdge : Int -> State -> State
insertLeftEdge =
    processLeftEdge EdgeSet.insert


removeLeftEdge : Int -> State -> State
removeLeftEdge =
    processLeftEdge EdgeSet.remove


setHelperOf : Int -> HelperVertex -> State -> State
setHelperOf edgeIndex helperVertex state =
    { state
        | helpers =
            Dict.insert edgeIndex helperVertex state.helpers
    }


handleStartVertex : Int -> State -> State
handleStartVertex index state =
    state
        |> insertLeftEdge index
        |> setHelperOf index (HelperVertex index index False)


updateAt : Int -> (a -> a) -> Array a -> Array a
updateAt index function array =
    case Array.get index array of
        Just item ->
            Array.set index (function item) array

        Nothing ->
            array


setPreviousEdge : Int -> Edge -> Edge
setPreviousEdge index edge =
    { edge | previousEdgeIndex = index }


setNextEdge : Int -> Edge -> Edge
setNextEdge index edge =
    { edge | nextEdgeIndex = index }


addDiagonal : Int -> HelperVertex -> State -> ( State, Int )
addDiagonal vertexIndex helperVertex state =
    let
        n =
            Array.length state.edges
    in
    Maybe.map4
        (\vi vj ei ej ->
            ( { state
                | edges =
                    state.edges
                        |> updateAt vertexIndex (setPreviousEdge (n + 1))
                        |> updateAt helperVertex.outgoingEdgeIndex (setPreviousEdge n)
                        |> updateAt ei.previousEdgeIndex (setNextEdge n)
                        |> updateAt ej.previousEdgeIndex (setNextEdge (n + 1))
                        |> Array.push
                            -- edge index n
                            { startVertexIndex = vertexIndex
                            , endVertexIndex = helperVertex.index
                            , previousEdgeIndex = ei.previousEdgeIndex
                            , nextEdgeIndex = helperVertex.outgoingEdgeIndex
                            }
                        |> Array.push
                            -- edge index n + 1
                            { startVertexIndex = helperVertex.index
                            , endVertexIndex = vertexIndex
                            , previousEdgeIndex = ej.previousEdgeIndex
                            , nextEdgeIndex = vertexIndex
                            }
              }
            , n
            )
        )
        (Array.get vertexIndex state.vertices)
        (Array.get helperVertex.index state.vertices)
        (Array.get vertexIndex state.edges)
        (Array.get helperVertex.outgoingEdgeIndex state.edges)
        |> defaultTo ( state, -1 )


getHelperOf : Int -> State -> Maybe HelperVertex
getHelperOf edgeIndex state =
    Dict.get edgeIndex state.helpers


handleEndVertex : Int -> State -> State
handleEndVertex index state =
    getEdge index state
        |> Maybe.andThen
            (\edge ->
                getHelperOf edge.previousEdgeIndex state
                    |> Maybe.map
                        (\helperVertex ->
                            let
                                diagonalAdded =
                                    if helperVertex.isMerge then
                                        state
                                            |> addDiagonal index helperVertex
                                            |> Tuple.first
                                    else
                                        state
                            in
                            diagonalAdded
                                |> removeLeftEdge edge.previousEdgeIndex
                        )
            )
        |> defaultTo state


getLeftEdge : Point2d -> State -> Maybe Int
getLeftEdge point state =
    EdgeSet.leftOf point state.edgeSet


handleSplitVertex : Int -> Point2d -> State -> State
handleSplitVertex index point state =
    getLeftEdge point state
        |> Maybe.andThen
            (\edgeIndex ->
                getHelperOf edgeIndex state
                    |> Maybe.map
                        (\helperVertex ->
                            let
                                ( updatedState, outgoingEdgeIndex ) =
                                    addDiagonal index helperVertex state
                            in
                            updatedState
                                |> setHelperOf edgeIndex (HelperVertex index outgoingEdgeIndex False)
                                |> insertLeftEdge index
                                |> setHelperOf index (HelperVertex index index False)
                        )
            )
        |> defaultTo state


handleMergeVertex : Int -> Point2d -> State -> State
handleMergeVertex index point state =
    getEdge index state
        |> Maybe.andThen
            (\edge ->
                getHelperOf edge.previousEdgeIndex state
                    |> Maybe.andThen
                        (\rightHelper ->
                            let
                                rightDiagonalAdded =
                                    if rightHelper.isMerge then
                                        state
                                            |> addDiagonal index rightHelper
                                            |> Tuple.first
                                    else
                                        state

                                rightUpdated =
                                    rightDiagonalAdded
                                        |> removeLeftEdge edge.previousEdgeIndex
                            in
                            getLeftEdge point rightUpdated
                                |> Maybe.andThen
                                    (\leftEdgeIndex ->
                                        getHelperOf leftEdgeIndex state
                                            |> Maybe.map
                                                (\leftHelper ->
                                                    let
                                                        ( leftDiagonalAdded, leftOutgoing ) =
                                                            if leftHelper.isMerge then
                                                                rightUpdated
                                                                    |> addDiagonal index leftHelper
                                                            else
                                                                ( rightUpdated, index )
                                                    in
                                                    leftDiagonalAdded
                                                        |> setHelperOf leftEdgeIndex (HelperVertex index leftOutgoing True)
                                                )
                                    )
                        )
            )
        |> defaultTo state


handleRightVertex : Int -> Point2d -> State -> State
handleRightVertex index point state =
    getLeftEdge point state
        |> Maybe.andThen
            (\leftEdgeIndex ->
                getHelperOf leftEdgeIndex state
                    |> Maybe.map
                        (\helperVertex ->
                            let
                                ( diagonalAdded, outgoingEdgeIndex ) =
                                    if helperVertex.isMerge then
                                        state
                                            |> addDiagonal index helperVertex
                                    else
                                        ( state, index )
                            in
                            diagonalAdded
                                |> setHelperOf leftEdgeIndex (HelperVertex index outgoingEdgeIndex False)
                        )
            )
        |> defaultTo state


handleLeftVertex : Int -> State -> State
handleLeftVertex index state =
    getEdge index state
        |> Maybe.andThen
            (\edge ->
                getHelperOf edge.previousEdgeIndex state
                    |> Maybe.map
                        (\helperVertex ->
                            let
                                diagonalAdded =
                                    if helperVertex.isMerge then
                                        state
                                            |> addDiagonal index helperVertex
                                            |> Tuple.first
                                    else
                                        state
                            in
                            diagonalAdded
                                |> removeLeftEdge edge.previousEdgeIndex
                                |> insertLeftEdge index
                                |> setHelperOf index (HelperVertex index index False)
                        )
            )
        |> defaultTo state


type alias MonotoneVertex =
    { index : Int
    , position : Point2d
    , nextVertexIndex : Int
    }


buildLoop : State -> Array Point2d -> Int -> Int -> ( Set Int, List MonotoneVertex ) -> ( Set Int, List MonotoneVertex )
buildLoop state points startIndex currentIndex ( processedEdgeIndices, accumulated ) =
    case getEdge currentIndex state of
        Just currentEdge ->
            case Array.get currentEdge.startVertexIndex points of
                Just vertexPosition ->
                    let
                        updatedEdgeIndices =
                            Set.insert currentIndex processedEdgeIndices

                        newVertex =
                            { index = currentEdge.startVertexIndex
                            , position = vertexPosition
                            , nextVertexIndex = currentEdge.endVertexIndex
                            }

                        newAccumulated =
                            newVertex :: accumulated

                        nextIndex =
                            currentEdge.nextEdgeIndex
                    in
                    if nextIndex == startIndex then
                        ( updatedEdgeIndices
                        , List.reverse newAccumulated
                        )
                    else
                        buildLoop
                            state
                            points
                            startIndex
                            nextIndex
                            ( updatedEdgeIndices, newAccumulated )

                Nothing ->
                    error ( processedEdgeIndices, [] )

        Nothing ->
            error ( processedEdgeIndices, [] )


collectMonotoneLoops : State -> ( Array Point2d, List (List MonotoneVertex) )
collectMonotoneLoops state =
    let
        points =
            state.vertices |> Array.map .position

        processStartEdge index accumulated =
            let
                ( processedEdgeIndices, accumulatedLoops ) =
                    accumulated
            in
            if Set.member index processedEdgeIndices then
                accumulated
            else
                let
                    ( updatedEdgeIndices, loop ) =
                        buildLoop state points index index ( processedEdgeIndices, [] )
                in
                ( updatedEdgeIndices, loop :: accumulatedLoops )

        allEdgeIndices =
            List.range 0 (Array.length state.edges - 1)

        ( _, loops ) =
            List.foldl processStartEdge ( Set.empty, [] ) allEdgeIndices
    in
    ( points, loops )


testPolygon : Polygon2d
testPolygon =
    let
        loop =
            List.map Point2d.fromCoordinates

        outerLoop =
            loop [ ( 0, 0 ), ( 5, 0 ), ( 5, 3 ), ( 0, 3 ) ]

        innerLoops =
            [ loop [ ( 1, 2 ), ( 2, 2 ), ( 2, 1 ), ( 1, 1 ) ]
            , loop [ ( 3, 2 ), ( 4, 1.5 ), ( 3, 1 ) ]
            ]
    in
    Polygon2d { outerLoop = outerLoop, innerLoops = innerLoops }


monotonePolygons : Polygon2d -> ( Array Point2d, List (List MonotoneVertex) )
monotonePolygons polygon =
    let
        { vertices, edges } =
            init polygon

        priorityQueue =
            vertices
                |> List.indexedMap Tuple.pair
                |> List.sortWith
                    (\( _, firstVertex ) ( _, secondVertex ) ->
                        comparePoints secondVertex.position firstVertex.position
                    )

        initialState =
            { edgeSet = EdgeSet.empty
            , helpers = Dict.empty
            , vertices = Array.fromList vertices
            , edges = edges
            }

        vertexArray =
            Array.fromList vertices

        handleVertex ( index, vertex ) current =
            case vertex.kind of
                Start ->
                    handleStartVertex index current

                End ->
                    handleEndVertex index current

                Right ->
                    handleRightVertex index vertex.position current

                Left ->
                    handleLeftVertex index current

                Split ->
                    handleSplitVertex index vertex.position current

                Merge ->
                    handleMergeVertex index vertex.position current

        finalState =
            List.foldl handleVertex initialState priorityQueue
    in
    collectMonotoneLoops finalState


type alias TriangulationState =
    { chainStart : MonotoneVertex
    , chainInterior : List MonotoneVertex
    , chainEnd : MonotoneVertex
    , faces : List ( Int, Int, Int )
    }


signedArea : MonotoneVertex -> MonotoneVertex -> MonotoneVertex -> Float
signedArea first second third =
    Triangle2d.counterclockwiseArea <|
        Triangle2d.fromVertices
            ( first.position
            , second.position
            , third.position
            )


addLeftChainVertex : MonotoneVertex -> TriangulationState -> TriangulationState
addLeftChainVertex vertex state =
    case state.chainInterior of
        [] ->
            if signedArea state.chainStart state.chainEnd vertex > 0 then
                let
                    newFace =
                        ( state.chainStart.index
                        , state.chainEnd.index
                        , vertex.index
                        )
                in
                { chainStart = state.chainStart
                , chainInterior = []
                , chainEnd = vertex
                , faces = newFace :: state.faces
                }
            else
                { chainStart = state.chainStart
                , chainInterior = [ state.chainEnd ]
                , chainEnd = vertex
                , faces = state.faces
                }

        firstInterior :: restInterior ->
            if signedArea firstInterior state.chainEnd vertex > 0 then
                let
                    newFace =
                        ( firstInterior.index
                        , state.chainEnd.index
                        , vertex.index
                        )
                in
                addLeftChainVertex vertex
                    { chainStart = state.chainStart
                    , chainInterior = restInterior
                    , chainEnd = firstInterior
                    , faces = newFace :: state.faces
                    }
            else
                { chainStart = state.chainStart
                , chainInterior = state.chainEnd :: state.chainInterior
                , chainEnd = vertex
                , faces = state.faces
                }


addRightChainVertex : MonotoneVertex -> TriangulationState -> TriangulationState
addRightChainVertex vertex state =
    case state.chainInterior of
        [] ->
            if signedArea vertex state.chainEnd state.chainStart > 0 then
                let
                    newFace =
                        ( vertex.index
                        , state.chainEnd.index
                        , state.chainStart.index
                        )
                in
                { chainStart = state.chainStart
                , chainInterior = []
                , chainEnd = vertex
                , faces = newFace :: state.faces
                }
            else
                { chainStart = state.chainStart
                , chainInterior = [ state.chainEnd ]
                , chainEnd = vertex
                , faces = state.faces
                }

        firstInterior :: restInterior ->
            if signedArea vertex state.chainEnd firstInterior > 0 then
                let
                    newFace =
                        ( vertex.index
                        , state.chainEnd.index
                        , firstInterior.index
                        )
                in
                addRightChainVertex vertex
                    { chainStart = state.chainStart
                    , chainInterior = restInterior
                    , chainEnd = firstInterior
                    , faces = newFace :: state.faces
                    }
            else
                { chainStart = state.chainStart
                , chainInterior = state.chainEnd :: state.chainInterior
                , chainEnd = vertex
                , faces = state.faces
                }


startNewRightChain : MonotoneVertex -> TriangulationState -> TriangulationState
startNewRightChain vertex state =
    let
        collectFaces firstVertex otherVertices accumulated =
            case otherVertices of
                [] ->
                    let
                        newFace =
                            ( firstVertex.index
                            , vertex.index
                            , state.chainStart.index
                            )
                    in
                    newFace :: accumulated

                firstOther :: restOther ->
                    let
                        newFace =
                            ( firstVertex.index
                            , vertex.index
                            , firstOther.index
                            )
                    in
                    collectFaces firstOther restOther (newFace :: accumulated)
    in
    { chainStart = state.chainEnd
    , chainInterior = []
    , chainEnd = vertex
    , faces = collectFaces state.chainEnd state.chainInterior state.faces
    }


startNewLeftChain : MonotoneVertex -> TriangulationState -> TriangulationState
startNewLeftChain vertex state =
    let
        collectFaces firstVertex otherVertices accumulated =
            case otherVertices of
                [] ->
                    let
                        newFace =
                            ( vertex.index
                            , firstVertex.index
                            , state.chainStart.index
                            )
                    in
                    newFace :: accumulated

                firstOther :: restOther ->
                    let
                        newFace =
                            ( vertex.index
                            , firstVertex.index
                            , firstOther.index
                            )
                    in
                    collectFaces firstOther restOther (newFace :: accumulated)
    in
    { chainStart = state.chainEnd
    , chainInterior = []
    , chainEnd = vertex
    , faces = collectFaces state.chainEnd state.chainInterior state.faces
    }


faces : List MonotoneVertex -> List ( Int, Int, Int )
faces vertices =
    let
        sortedVertices =
            vertices
                |> List.sortWith
                    (\first second ->
                        comparePoints second.position first.position
                    )
    in
    case sortedVertices of
        [] ->
            []

        [ single ] ->
            []

        first :: second :: rest ->
            let
                initialState =
                    { chainStart = first
                    , chainInterior = []
                    , chainEnd = second
                    , faces = []
                    }

                processVertex vertex state =
                    if vertex.nextVertexIndex == state.chainStart.index then
                        -- new vertex on the right will connect to all chain
                        -- vertices on the left
                        startNewRightChain vertex state
                    else if state.chainStart.nextVertexIndex == vertex.index then
                        -- new vertex on the left will connect to all chain
                        -- vertices on the right
                        startNewLeftChain vertex state
                    else if vertex.nextVertexIndex == state.chainEnd.index then
                        -- continuing left chain
                        addRightChainVertex vertex state
                    else if state.chainEnd.nextVertexIndex == vertex.index then
                        -- continuing right chain
                        addLeftChainVertex vertex state
                    else
                        error state
            in
            List.foldl processVertex initialState rest |> .faces


triangulation : Polygon2d -> TriangularMesh Point2d
triangulation polygon =
    let
        ( points, loops ) =
            monotonePolygons polygon
    in
    TriangularMesh.indexed points (List.map faces loops |> List.concat)
