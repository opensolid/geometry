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


{-| A parameter value between 0 and 1. Curve types such as [`Arc2d`](Arc2d) and
[`CubicSpline3d`](CubicSpline3d) use `ParameterValue` arguments for curve
evaluation functions such as [`Arc2d.pointOn`](Arc2d#pointOn) and
[`CubicSpline3d.samplesAt`](CubicSpline3d#samplesAt).
-}
type ParameterValue
    = ParameterValue Float


{-| The parameter value 0.

    ParameterValue.value ParameterValue.zero
    --> 0

-}
zero : ParameterValue
zero =
    ParameterValue 0


{-| The parameter value 0.5.

    ParameterValue.value ParameterValue.half
    --> 0.5

-}
half : ParameterValue
half =
    ParameterValue 0.5


{-| The parameter value 1.

    ParameterValue.value ParameterValue.one
    --> 1

-}
one : ParameterValue
one =
    ParameterValue 1


{-| Construct a valid parameter value by clamping a plain `Float` value to
between 0 and 1.

    ParameterValue.value (ParameterValue.clamped 0.75)
    --> 0.75

    ParameterValue.value (ParameterValue.clamped -0.25)
    --> 0

    ParameterValue.value (ParameterValue.clamped 1.25)
    --> 1

-}
clamped : Float -> ParameterValue
clamped givenValue =
    if isNaN givenValue then
        ParameterValue givenValue
    else
        ParameterValue (clamp 0 1 givenValue)


{-| If the given value is between 0 and 1, return `Just` that value as a
`ParameterValue`. Otherwise, return `Nothing`.

    ParameterValue.checked 0.75
        |> Maybe.map ParameterValue.value
    --> Just 0.75

    ParameterValue.checked -0.25
    --> Nothing

    ParameterValue.checked 1.25
    --> Nothing

-}
checked : Float -> Maybe ParameterValue
checked givenValue =
    if isNaN givenValue then
        Nothing
    else if 0 <= givenValue && givenValue <= 1 then
        Just (ParameterValue givenValue)
    else
        Nothing


{-| Directly construct a `ParameterValue` from a `Float` without checking
whether it is valid. `ParameterValue.clamped` should generally be used instead,
unless you are **very** sure you know what you are doing and
profiling/benchmarking shows that `ParameterValue.clamped` is a performance
bottleneck.
-}
unsafe : Float -> ParameterValue
unsafe =
    ParameterValue


{-| Find the midpoint between two parameter values.

    ParameterValue.midpoint
        ParameterValue.zero
        ParameterValue.one
    --> ParameterValue.half

-}
midpoint : ParameterValue -> ParameterValue -> ParameterValue
midpoint (ParameterValue firstValue) (ParameterValue secondValue) =
    ParameterValue (firstValue + (secondValue - firstValue) / 2)


{-| Subtract a parameter value from 1 to give a new parameter value.

    ParameterValue.oneMinus ParameterValue.one
    --> ParameterValue.zero

    ParameterValue.oneMinus ParameterValue.zero
    --> ParameterValue.one

    ParameterValue.oneMinus ParameterValue.half
    --> ParameterValue.half

This can be thought of as the 'negation' or 'complement' of a parameter value.
For example, evaluating a reversed curve at a parameter value `t` is generally
equivalent to evaluating the original curve at a parameter value
<code>1&nbsp;-&nbsp;t</code>, and vice versa.

-}
oneMinus : ParameterValue -> ParameterValue
oneMinus (ParameterValue value_) =
    ParameterValue (1 - value_)


{-| Construct a list of parameter values by taking a given number of steps from
0 to 1. Note that the number of returned values will in general be one greater
than the number of steps!

    ParameterValue.steps 0
    --> []

    ParameterValue.steps 1
    --> [ ParameterValue.zero, ParameterValue.one ]

    Parametervalue.steps 2
    --> [ ParameterValue.zero
    --> , ParameterValue.half
    --> , ParameterValue.one
    --> ]

    ParameterValue.steps 5
        |> List.map ParameterValue.value
    --> [ 0, 0.2, 0.4, 0.6, 0.8, 1 ]

-}
steps : Int -> List ParameterValue
steps n =
    if n < 1 then
        []
    else
        endpointsHelp 0 n (toFloat n) []


{-| Construct a list of parameter values by dividing the range [0,1] into a
given number of steps and then returning the value at the beginning of each
step.

    ParameterValue.leading 0
    --> []

    ParameterValue.leading 1
    --> [ ParameterValue.zero ]

    Parametervalue.leading 2
    --> [ ParameterValue.zero, ParameterValue.half ]

    ParameterValue.leading 5
        |> List.map ParameterValue.value
    --> [ 0, 0.2, 0.4, 0.6, 0.8 ]

-}
leading : Int -> List ParameterValue
leading n =
    if n < 1 then
        []
    else
        endpointsHelp 0 (n - 1) (toFloat n) []


{-| Construct a list of parameter values by dividing the range [0,1] into a
given number of steps and then returning the value at the end of each step.

    ParameterValue.trailing 0
    --> []

    ParameterValue.trailing 1
    --> [ ParameterValue.one ]

    Parametervalue.trailing 2
    --> [ ParameterValue.half, ParameterValue.one ]

    ParameterValue.trailing 5
        |> List.map ParameterValue.value
    --> [ 0.2, 0.4, 0.6, 0.8, 1 ]

-}
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


{-| Construct a list of parameter values by dividing the range [0,1] into a
given number of steps and then returning the value at the midpoint of each step.

    ParameterValue.midpoints 0
    --> []

    ParameterValue.midpoints 1
    --> [ ParameterValue.half ]

    ParameterValue.midpoints 2
        |> List.map ParameterValue.value
    --> [ 0.25, 0.75 ]

    ParameterValue.midpoints 5
        |> List.map ParameterValue.value
    --> [ 0.1, 0.3, 0.5, 0.7, 0.9 ]

-}
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


{-| Construct a list of evenly-spaced parameter values between 0 and 1 by
specifying:

  - the number of steps to take from 0 to 1
  - whether to include the start value (0)
  - whether to include the end value (1)

This is a more general form of `steps`, `leading` and `trailing`; for example,

    ParameterValue.steps 10

is equivalent to

    ParameterValue.range
        { numSteps = 10
        , includeStart = True
        , includeEnd = True
        }

and

    ParameterValue.trailing 10

is equivalent to

    ParameterValue.range
        { numSteps = 10
        , includeStart = False
        , includeEnd = True
        }

-}
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


{-| Convert a `ParameterValue` to a plain `Float` value between 0 and 1.

    ParameterValue.value ParameterValue.half
    --> 0.5

-}
value : ParameterValue -> Float
value (ParameterValue value_) =
    value_
