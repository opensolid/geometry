module EllipticalArc2d.FromEndpoints exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Char
import Direction2d exposing (Direction2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Html exposing (Html)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Interaction as Interaction
import Svg.Interaction.ScrollAmount as ScrollAmount exposing (ScrollAmount)
import Vector2d


boundingBox : BoundingBox2d
boundingBox =
    BoundingBox2d.fromExtrema
        { minX = 0
        , minY = 0
        , maxX = 500
        , maxY = 400
        }


type Target
    = CenterPoint
    | StartPoint
    | EndPoint
    | XDirection
    | Section EllipticalArc2d.SweptAngle
    | XRadius
    | YRadius
    | Elsewhere


type alias Model =
    { startPoint : Point2d
    , endPoint : Point2d
    , xDirection : Direction2d
    , xRadius : Float
    , yRadius : Float
    , sweptAngle : EllipticalArc2d.SweptAngle
    , interactionModel : Interaction.Model Target
    , dragRotationCenter : Maybe Point2d
    , selected : Maybe Target
    }


type Msg
    = InteractionMsg (Interaction.Msg Target)


init : ( Model, Cmd Msg )
init =
    ( { startPoint = Point2d.fromCoordinates ( 400, 150 )
      , endPoint = Point2d.fromCoordinates ( 250, 300 )
      , xDirection = Direction2d.x
      , xRadius = 200
      , yRadius = 100
      , sweptAngle = EllipticalArc2d.smallPositive
      , interactionModel = Interaction.model
      , dragRotationCenter = Nothing
      , selected = Nothing
      }
    , Cmd.none
    )


currrentDragCenter : Model -> Maybe Point2d
currrentDragCenter model =
    case model.dragRotationCenter of
        Just existingCenter ->
            Just existingCenter

        Nothing ->
            constructArc model
                |> Maybe.map
                    EllipticalArc2d.centerPoint


drag : Target -> Point2d -> Point2d -> Model -> Model
drag target previousPoint currentPoint model =
    let
        displacement =
            Vector2d.from previousPoint currentPoint
    in
    case target of
        StartPoint ->
            { model
                | startPoint =
                    Point2d.translateBy displacement
                        model.startPoint
            }

        EndPoint ->
            { model
                | endPoint =
                    Point2d.translateBy displacement
                        model.endPoint
            }

        CenterPoint ->
            { model
                | startPoint =
                    Point2d.translateBy displacement
                        model.startPoint
                , endPoint =
                    Point2d.translateBy displacement
                        model.endPoint
            }

        XDirection ->
            let
                currentCenter =
                    currrentDragCenter model

                rotatedDirection =
                    case currentCenter of
                        Just centerPoint ->
                            let
                                rotationAngle =
                                    Interaction.rotationAround centerPoint
                                        previousPoint
                                        currentPoint
                            in
                            Direction2d.rotateBy rotationAngle model.xDirection

                        Nothing ->
                            model.xDirection
            in
            { model
                | dragRotationCenter = currentCenter
                , xDirection = rotatedDirection
            }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update (InteractionMsg interactionMsg) model =
    let
        ( updatedInteractionModel, interactions ) =
            Interaction.update interactionMsg model.interactionModel

        handleInteraction interaction model =
            case interaction of
                Interaction.Drag target _ { previousPoint, currentPoint } ->
                    drag target previousPoint currentPoint model

                Interaction.Release XDirection _ _ ->
                    { model | dragRotationCenter = Nothing }

                Interaction.Click target modifiers ->
                    case target of
                        Section sweptAngle ->
                            { model
                                | sweptAngle = sweptAngle
                                , selected = Nothing
                            }

                        XRadius ->
                            { model | selected = Just XRadius }

                        YRadius ->
                            { model | selected = Just YRadius }

                        _ ->
                            { model | selected = Nothing }

                Interaction.Tap target ->
                    case target of
                        Section sweptAngle ->
                            { model | sweptAngle = sweptAngle }

                        _ ->
                            model

                Interaction.Slide target { previousPoint, currentPoint } ->
                    drag target previousPoint currentPoint model

                Interaction.Scroll target modifiers scrollAmount ->
                    let
                        scale =
                            if ScrollAmount.isPositive scrollAmount then
                                1 / 1.1
                            else
                                1.1
                    in
                    case model.selected of
                        Just XRadius ->
                            { model | xRadius = scale * model.xRadius }

                        Just YRadius ->
                            { model | yRadius = scale * model.yRadius }

                        _ ->
                            model

                Interaction.Lift XDirection ->
                    { model | dragRotationCenter = Nothing }

                _ ->
                    model
    in
    ( List.foldl handleInteraction
        { model | interactionModel = updatedInteractionModel }
        interactions
    , Cmd.none
    )


noFill : Svg.Attribute msg
noFill =
    Svg.Attributes.fill "none"


whiteFill : Svg.Attribute msg
whiteFill =
    Svg.Attributes.fill "white"


blackFill : Svg.Attribute msg
blackFill =
    Svg.Attributes.fill "black"


blackStroke : Svg.Attribute msg
blackStroke =
    Svg.Attributes.stroke "black"


lightStroke : Svg.Attribute msg
lightStroke =
    Svg.Attributes.stroke "rgba(0, 0, 0, 0.25)"


blueStroke : Svg.Attribute msg
blueStroke =
    Svg.Attributes.stroke "blue"


lightGreyStroke : Svg.Attribute msg
lightGreyStroke =
    Svg.Attributes.stroke "lightgrey"


transparentStroke : Svg.Attribute msg
transparentStroke =
    Svg.Attributes.stroke "transparent"


thickStroke : Svg.Attribute msg
thickStroke =
    Svg.Attributes.strokeWidth "10"


roundCap : Svg.Attribute msg
roundCap =
    Svg.Attributes.strokeLinecap "round"


dashed : Svg.Attribute msg
dashed =
    Svg.Attributes.strokeDasharray "5 5"


pointHandle : Target -> Point2d -> Svg Msg
pointHandle target point =
    Interaction.pointHandle point
        { target = target
        , radius = 15
        }
        |> Svg.map InteractionMsg


sectionHandle : EllipticalArc2d.SweptAngle -> EllipticalArc2d -> Svg Msg
sectionHandle sweptAngle arc =
    let
        shape =
            Svg.ellipticalArc2d
                [ noFill, transparentStroke, thickStroke, roundCap ]
                arc
    in
    Interaction.customHandle shape (Section sweptAngle)
        |> Svg.map InteractionMsg


lineSegmentHandle : Target -> LineSegment2d -> Svg Msg
lineSegmentHandle target lineSegment =
    Interaction.lineSegmentHandle lineSegment
        { target = target
        , padding = 15
        }
        |> Svg.map InteractionMsg


constructArc : Model -> Maybe EllipticalArc2d
constructArc model =
    EllipticalArc2d.fromEndpoints
        { startPoint = model.startPoint
        , endPoint = model.endPoint
        , xDirection = model.xDirection
        , xRadius = model.xRadius
        , yRadius = model.yRadius
        , sweptAngle = model.sweptAngle
        }


isActive : Target -> Model -> Bool
isActive target model =
    Interaction.isHovering target model.interactionModel
        || (model.selected == Just target)


sweptAngleString : EllipticalArc2d.SweptAngle -> String
sweptAngleString sweptAngle =
    toString sweptAngle
        |> String.uncons
        |> Maybe.map (\( first, rest ) -> String.cons (Char.toLower first) rest)
        |> Maybe.withDefault ""


view : Model -> Html Msg
view model =
    let
        properties =
            { startPoint = model.startPoint
            , endPoint = model.endPoint
            , xRadius = model.xRadius
            , yRadius = model.yRadius
            , xDirection = model.xDirection
            , sweptAngle = model.sweptAngle
            }

        computedArc =
            EllipticalArc2d.fromEndpoints properties

        sweptAngleTypes =
            [ EllipticalArc2d.smallPositive
            , EllipticalArc2d.smallNegative
            , EllipticalArc2d.largePositive
            , EllipticalArc2d.largeNegative
            ]

        dashedEllipses =
            sweptAngleTypes
                |> List.map
                    (\sweptAngle ->
                        { properties | sweptAngle = sweptAngle }
                    )
                |> List.map EllipticalArc2d.fromEndpoints
                |> List.map2 (,) sweptAngleTypes
                |> List.filterMap
                    (\( sweptAngle, computedArc ) ->
                        computedArc |> Maybe.map (\arc -> ( arc, sweptAngle ))
                    )
                |> List.map
                    (\( arc, sweptAngle ) ->
                        Svg.g []
                            [ Svg.ellipticalArc2d
                                [ noFill, dashed, lightGreyStroke ]
                                arc
                            , sectionHandle sweptAngle arc
                            ]
                    )
                |> Svg.g []

        ellipseSvg =
            case computedArc of
                Just arc ->
                    let
                        centerPoint =
                            EllipticalArc2d.centerPoint arc

                        xPoint =
                            centerPoint
                                |> Point2d.translateBy
                                    (Vector2d.withLength model.xRadius
                                        model.xDirection
                                    )

                        yPoint =
                            centerPoint
                                |> Point2d.translateBy
                                    (Vector2d.withLength model.yRadius
                                        (Direction2d.perpendicularTo
                                            model.xDirection
                                        )
                                    )

                        xRadialLine =
                            LineSegment2d.from centerPoint xPoint

                        yRadialLine =
                            LineSegment2d.from centerPoint yPoint

                        xStroke =
                            if isActive XRadius model then
                                blueStroke
                            else
                                lightStroke

                        yStroke =
                            if isActive YRadius model then
                                blueStroke
                            else
                                lightStroke

                        parameterized =
                            EllipticalArc2d.arcLengthParameterized 0.5 arc

                        arcLength =
                            EllipticalArc2d.arcLength parameterized

                        midPoint =
                            EllipticalArc2d.pointAlong parameterized
                                (0.5 * arcLength)

                        midTangent =
                            EllipticalArc2d.tangentAlong parameterized
                                (0.5 * arcLength)

                        directionIndicator =
                            Maybe.map2
                                (\point direction ->
                                    Svg.direction2dWith
                                        { length = 10
                                        , tipLength = 10
                                        , tipWidth = 10
                                        }
                                        [ blackStroke, blackFill ]
                                        point
                                        direction
                                )
                                midPoint
                                midTangent
                                |> Maybe.withDefault (Svg.text "")
                    in
                    Svg.g []
                        [ dashedEllipses
                        , Svg.g [ noFill ]
                            [ Svg.lineSegment2d [ xStroke ] xRadialLine
                            , Svg.lineSegment2d [ yStroke ] yRadialLine
                            , lineSegmentHandle XRadius xRadialLine
                            , lineSegmentHandle YRadius yRadialLine
                            ]
                        , Svg.ellipticalArc2d [ noFill, blackStroke ] arc
                        , directionIndicator
                        , Svg.direction2d [ noFill, blackStroke ]
                            centerPoint
                            model.xDirection
                        , Svg.point2d [ whiteFill, blackStroke ] centerPoint
                        , pointHandle CenterPoint centerPoint
                        ]

                Nothing ->
                    Svg.text ""

        anchoredDirection =
            case model.dragRotationCenter of
                Just centerPoint ->
                    Svg.g [ lightGreyStroke, whiteFill ]
                        [ Svg.direction2d [] centerPoint model.xDirection
                        , Svg.point2d [] centerPoint
                        ]

                Nothing ->
                    Svg.text ""

        directionTipHandle =
            case currrentDragCenter model of
                Just currentCenter ->
                    Interaction.directionTipHandle
                        currentCenter
                        model.xDirection
                        { length = 50
                        , tipLength = 10
                        , tipWidth = 10
                        , target = XDirection
                        , padding = 15
                        }
                        |> Svg.map InteractionMsg

                Nothing ->
                    Svg.text ""

        { maxX, minY } =
            BoundingBox2d.extrema boundingBox
    in
    Svg.render2d boundingBox <|
        Interaction.container InteractionMsg
            { target = Elsewhere
            , renderBounds = boundingBox
            }
            [ ellipseSvg
            , anchoredDirection
            , directionTipHandle
            , Svg.point2d [ whiteFill, blackStroke ] model.startPoint
            , pointHandle StartPoint model.startPoint
            , Svg.point2d [ whiteFill, blackStroke ] model.endPoint
            , pointHandle EndPoint model.endPoint
            , Svg.boundingBox2d
                [ Svg.Attributes.stroke "rgb(238, 238, 238)"
                , Svg.Attributes.rx "6"
                , Svg.Attributes.ry "6"
                , noFill
                ]
                (BoundingBox2d.fromExtrema
                    { minX = BoundingBox2d.minX boundingBox + 0.5
                    , minY = BoundingBox2d.minY boundingBox + 0.5
                    , maxX = BoundingBox2d.maxX boundingBox - 0.5
                    , maxY = BoundingBox2d.maxY boundingBox - 0.5
                    }
                )
            , Svg.text2d
                [ Svg.Attributes.textAnchor "end"
                , Svg.Attributes.alignmentBaseline "baseline"
                , Svg.Attributes.fontFamily "monospace"
                ]
                (Point2d.fromCoordinates ( maxX - 10, minY + 10 ))
                ("sweptAngle = EllipticalArc2d." ++ sweptAngleString model.sweptAngle)
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Interaction.subscriptions model.interactionModel |> Sub.map InteractionMsg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
