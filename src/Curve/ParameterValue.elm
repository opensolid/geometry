module Curve.ParameterValue
    exposing
        ( ParameterValue
        , checked
        , clamped
        , half
        , leading
        , midpoint
        , midpoints
        , one
        , oneMinus
        , range
        , steps
        , trailing
        , unsafe
        , value
        , zero
        )

{-| Curves in `opensolid/geometry` are [parameterized](https://en.wikipedia.org/wiki/Parametric_equation)
by a value that ranges from 0 to 1. A value of 0 corresponds to the start point
of the curve and a value of 1 corresponds to the end point. This module contains
functionality for:

  - Constructing parameter values that are guaranteed to be in the range 0 to 1
  - Constructing ranges of evenly-spaced parameter values

@docs ParameterValue


# Constants

@docs zero, half, one


# Conversion to and from `Float` values

@docs value, clamped, checked, unsafe


# Ranges

@docs steps, leading, trailing, midpoints, range


# Arithmetic

@docs midpoint, oneMinus

-}


{-| -}
type ParameterValue
    = ParameterValue Float


{-| -}
zero : ParameterValue
zero =
    ParameterValue 0


{-| -}
half : ParameterValue
half =
    ParameterValue 0.5


{-| -}
one : ParameterValue
one =
    ParameterValue 1


{-| -}
clamped : Float -> ParameterValue
clamped givenValue =
    if isNaN givenValue then
        ParameterValue givenValue
    else
        ParameterValue (clamp 0 1 givenValue)


{-| -}
checked : Float -> Maybe ParameterValue
checked givenValue =
    if isNaN givenValue then
        Nothing
    else if 0 <= givenValue && givenValue <= 1 then
        Just (ParameterValue givenValue)
    else
        Nothing


{-| -}
unsafe : Float -> ParameterValue
unsafe =
    ParameterValue


{-| -}
midpoint : ParameterValue -> ParameterValue -> ParameterValue
midpoint (ParameterValue firstValue) (ParameterValue secondValue) =
    ParameterValue (firstValue + (secondValue - firstValue) / 2)


{-| -}
oneMinus : ParameterValue -> ParameterValue
oneMinus (ParameterValue value_) =
    ParameterValue (1 - value_)


{-| -}
steps : Int -> List ParameterValue
steps n =
    if n < 1 then
        []
    else
        endpointsHelp 0 n (toFloat n) []


{-| -}
leading : Int -> List ParameterValue
leading n =
    if n < 1 then
        []
    else
        endpointsHelp 0 (n - 1) (toFloat n) []


{-| -}
trailing : Int -> List ParameterValue
trailing n =
    if n < 1 then
        []
    else
        endpointsHelp 1 n (toFloat n) []


endpointsHelp : Int -> Int -> Float -> List ParameterValue -> List ParameterValue
endpointsHelp startIndex index divisor accumulated =
    let
        parameterValue =
            ParameterValue (toFloat index / divisor)

        newAccumulated =
            parameterValue :: accumulated
    in
    if index == startIndex then
        newAccumulated
    else
        endpointsHelp startIndex (index - 1) divisor newAccumulated


{-| -}
midpoints : Int -> List ParameterValue
midpoints n =
    if n < 1 then
        []
    else
        midpointsHelp (2 * n - 1) (2 * toFloat n) []


midpointsHelp : Int -> Float -> List ParameterValue -> List ParameterValue
midpointsHelp index divisor accumulated =
    let
        parameterValue =
            ParameterValue (toFloat index / divisor)

        newAccumulated =
            parameterValue :: accumulated
    in
    if index == 1 then
        newAccumulated
    else
        midpointsHelp (index - 2) divisor newAccumulated


{-| -}
range : { numSteps : Int, includeStart : Bool, includeEnd : Bool } -> List ParameterValue
range { numSteps, includeStart, includeEnd } =
    if numSteps < 1 then
        []
    else
        let
            startIndex =
                if includeStart then
                    0
                else
                    1

            endIndex =
                if includeEnd then
                    numSteps
                else
                    numSteps - 1
        in
        if startIndex <= endIndex then
            endpointsHelp startIndex endIndex (toFloat numSteps) []
        else
            []


{-| -}
value : ParameterValue -> Float
value (ParameterValue value_) =
    value_
