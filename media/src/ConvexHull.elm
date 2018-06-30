module ConvexHull exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Drawing2d
import Html exposing (Html)
import Html.Events
import Point2d exposing (Point2d)
import Polygon2d
import Random exposing (Generator)


type alias Model =
    { points : List Point2d
    }


type Msg
    = Click
    | NewRandomPoints (List Point2d)


renderBounds : BoundingBox2d
renderBounds =
    BoundingBox2d.fromExtrema
        { minX = 0
        , maxX = 300
        , minY = 0
        , maxY = 300
        }


pointsGenerator : Generator (List Point2d)
pointsGenerator =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema renderBounds

        pointGenerator =
            Random.map2 (\x y -> Point2d.fromCoordinates ( x, y ))
                (Random.float (minX + 30) (maxX - 30))
                (Random.float (minY + 30) (maxY - 30))
    in
    Random.int 2 32
        |> Random.andThen
            (\listSize -> Random.list listSize pointGenerator)


generateNewPoints : Cmd Msg
generateNewPoints =
    Random.generate NewRandomPoints pointsGenerator


init : ( Model, Cmd Msg )
init =
    ( { points = [] }, generateNewPoints )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Click ->
            ( model, generateNewPoints )

        NewRandomPoints points ->
            ( { model | points = points }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        convexHull =
            Polygon2d.convexHull model.points
    in
    Html.div [ Html.Events.onClick Click ]
        [ Drawing2d.toHtml renderBounds [] <|
            [ Drawing2d.polygon convexHull
            , Drawing2d.dots model.points
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
