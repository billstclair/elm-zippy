----------------------------------------------------------------------
--
-- SharedTypes.elm
-- Types used everywhere.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module SharedTypes
    exposing
        ( Msg(..)
        , Object
        , Vector
        , makeSize
        , makeVector
        , zeroVector
        )

import Window exposing (Size)


type Msg
    = Resize Size
    | Nop


type alias Vector =
    { x : Float
    , y : Float
    }


makeVector : Float -> Float -> Vector
makeVector x y =
    { x = x, y = y }


zeroVector : Vector
zeroVector =
    { x = 0
    , y = 0
    }


makeSize : Int -> Int -> Size
makeSize w h =
    { width = w, height = h }


{-| For now, objects are all rectangular
-}
type alias Object =
    { size : Size
    , image : Maybe Int
    , position : Vector
    , velocity : Vector
    , mass : Float
    }
