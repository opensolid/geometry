module Arc.SweptAngle
    exposing
        ( SweptAngle
        , largeNegative
        , largePositive
        , smallNegative
        , smallPositive
        )

{-| When constructing circular or elliptical arcs, it is sometimes necessary to
specify which of several possible arcs you want. For example, if you ask for a
circular arc from the point (1, 0) to the point (0, 1) with a radius of 1, there
are four possible solutions:

  - An arc with a swept angle of 90 degrees, with center point at (0, 0)
  - An arc with a swept angle of -270 degrees, with center point at (0, 0)
  - An arc with a swept angle of -90 degrees, with center point at (1, 1)
  - An Arc with a swept angle of 270 degrees, with center point at (1, 1)

The `SweptAngle` type is used in these cases to specify which arc you want.

@docs SweptAngle, smallPositive, smallNegative, largePositive, largeNegative

-}

import Geometry.Types as Types


{-| -}
type alias SweptAngle =
    Types.SweptAngle


{-| -}
smallPositive : SweptAngle
smallPositive =
    Types.SmallPositive


{-| -}
smallNegative : SweptAngle
smallNegative =
    Types.SmallNegative


{-| -}
largePositive : SweptAngle
largePositive =
    Types.LargePositive


{-| -}
largeNegative : SweptAngle
largeNegative =
    Types.LargeNegative
