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


module Zippy.SharedTypes
    exposing
        ( Msg(..)
        , Object
        , Vector
        , makeSize
        , makeVector
        , sizeToVector
        , zeroVector
        )

import Window exposing (Size)


type Msg
    = Resize Size
    | Update
    | Nop


type alias Vector =
    { x : Float
    , y : Float
    }


makeVector : Float -> Float -> Vector
makeVector x y =
    { x = x, y = y }


sizeToVector : Size -> Vector
sizeToVector size =
    { x = toFloat size.width
    , y = toFloat size.height
    }


zeroVector : Vector
zeroVector =
    makeVector 0 0


makeSize : Int -> Int -> Size
makeSize w h =
    { width = w, height = h }


{-| For now, objects are all rectangular
-}
type alias Object =
    { size : Vector
    , image : Maybe String
    , rightImage : Maybe String
    , position : Vector
    , velocity : Vector
    , mass : Float
    }
