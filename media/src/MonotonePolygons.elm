module MonotonePolygones exposing (..)

import Array.Hamt as Array
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Kintail.InputWidget as InputWidget
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)
import OpenSolid.Polygon2d.Monotone as Monotone
import OpenSolid.Svg as Svg
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Random exposing (Generator)
import Svg
import Svg.Attributes


type alias Model =
    { polygon : Polygon2d
    , angleInDegrees : Float
    , showTriangulation : Bool
    , showMonotonePolygons : Bool
    }


type Msg
    = Click
    | NewPolygon Polygon2d
    | SetAngleInDegrees Float
    | SetShowTriangulation Bool
    | SetShowMonotonePolygons Bool


renderBounds : BoundingBox2d
renderBounds =
    BoundingBox2d.fromExtrema
        { minX = 0
        , maxX = 800
        , minY = 0
        , maxY = 600
        }


polygonGenerator : Generator Polygon2d
polygonGenerator =
    let
        centerPoint =
            BoundingBox2d.centroid renderBounds

        ( width, height ) =
            BoundingBox2d.dimensions renderBounds

        minRadius =
            10

        maxRadius =
            0.5 * min width height - 10

        midRadius =
            (minRadius + maxRadius) / 2

        innerRadiusGenerator =
            Random.float minRadius (midRadius - 5)

        outerRadiusGenerator =
            Random.float (midRadius + 5) maxRadius
    in
    Random.int 3 32
        |> Random.andThen
            (\numPoints ->
                Random.list numPoints
                    (Random.pair innerRadiusGenerator outerRadiusGenerator)
                    |> Random.map
                        (List.indexedMap
                            (\index ( innerRadius, outerRadius ) ->
                                let
                                    angle =
                                        turns 1
                                            * toFloat index
                                            / toFloat numPoints

                                    innerRadialVector =
                                        Vector2d.fromPolarComponents
                                            ( innerRadius
                                            , angle
                                            )

                                    outerRadialVector =
                                        Vector2d.fromPolarComponents
                                            ( outerRadius
                                            , angle
                                            )

                                    innerPoint =
                                        centerPoint
                                            |> Point2d.translateBy
                                                innerRadialVector

                                    outerPoint =
                                        centerPoint
                                            |> Point2d.translateBy
                                                outerRadialVector
                                in
                                ( innerPoint, outerPoint )
                            )
                        )
                    |> Random.map List.unzip
                    |> Random.map
                        (\( innerLoop, outerLoop ) ->
                            Polygon2d.withHoles outerLoop
                                [ List.reverse innerLoop ]
                        )
            )


generateNewPolygon : Cmd Msg
generateNewPolygon =
    Random.generate NewPolygon polygonGenerator


init : ( Model, Cmd Msg )
init =
    ( { polygon = Polygon2d.singleLoop []
      , angleInDegrees = 0
      , showTriangulation = False
      , showMonotonePolygons = False
      }
    , generateNewPolygon
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPolygon )

        NewPolygon polygon ->
            ( { model | polygon = polygon }, Cmd.none )

        SetAngleInDegrees angleInDegrees ->
            ( { model | angleInDegrees = angleInDegrees }, Cmd.none )

        SetShowTriangulation showTriangulation ->
            ( { model | showTriangulation = showTriangulation }, Cmd.none )

        SetShowMonotonePolygons showMonotonePolygons ->
            ( { model | showMonotonePolygons = showMonotonePolygons }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( width, height ) =
            BoundingBox2d.dimensions renderBounds

        rotatedPolygon =
            Polygon2d.rotateAround (BoundingBox2d.centroid renderBounds)
                (degrees model.angleInDegrees)
                model.polygon

        ( points, loops ) =
            Monotone.monotonePolygons rotatedPolygon

        numLoops =
            List.length loops

        drawLoop loopIndex vertices =
            let
                hueString =
                    toString (360 * toFloat loopIndex / toFloat numLoops)

                fillColor =
                    if model.showMonotonePolygons then
                        "hsla(" ++ hueString ++ ",50%, 50%, 0.5)"
                    else
                        "rgb(248, 248, 248)"

                strokeColor =
                    if model.showMonotonePolygons then
                        "hsla(" ++ hueString ++ ",50%, 40%, 0.5)"
                    else
                        "darkgrey"

                faceIndices =
                    Monotone.faces vertices

                triangles =
                    faceIndices
                        |> List.filterMap
                            (\( i, j, k ) ->
                                Maybe.map3
                                    (\p1 p2 p3 ->
                                        Triangle2d.fromVertices ( p1, p2, p3 )
                                    )
                                    (Array.get i points)
                                    (Array.get j points)
                                    (Array.get k points)
                            )
            in
            triangles
                |> List.map
                    (Svg.triangle2d
                        [ Svg.Attributes.fill fillColor
                        , Svg.Attributes.stroke strokeColor
                        ]
                    )
                |> Svg.g []
    in
    Html.div []
        [ Html.div [ Html.Events.onClick Click ]
            [ Svg.render2d renderBounds <|
                Svg.g []
                    [ if model.showTriangulation then
                        Svg.g [] (List.indexedMap drawLoop loops)
                      else
                        Svg.text ""
                    , Svg.polygon2d
                        [ Svg.Attributes.fill "none"
                        , Svg.Attributes.stroke "black"
                        ]
                        rotatedPolygon
                    ]
            ]
        , InputWidget.slider
            [ Html.Attributes.style [ ( "width", toString width ++ "px" ) ] ]
            { min = -180, max = 180, step = 1 }
            model.angleInDegrees
            |> Html.map SetAngleInDegrees
        , Html.div []
            [ InputWidget.checkbox [] model.showTriangulation
                |> Html.map SetShowTriangulation
            , Html.text "Show triangulation"
            ]
        , Html.div []
            [ InputWidget.checkbox
                [ Html.Attributes.disabled (not model.showTriangulation) ]
                model.showMonotonePolygons
                |> Html.map SetShowMonotonePolygons
            , Html.text "Show monotone polygons"
            ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
