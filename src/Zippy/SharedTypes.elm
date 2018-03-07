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
        ( ImageUrls
        , Msg(..)
        , Object
        , Vector
        , makeSize
        , makeVector
        , sizeToVector
        , zeroVector
        )

import Time exposing (Time)
import Window exposing (Size)


type Msg
    = Resize Size
    | Initialize Time
    | Update
    | ShowDialog Bool
    | Run Bool
    | Clear
    | RemoveObject Object
    | AddObject Object
    | SelectObject Object
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


type alias ImageUrls =
    { right : String
    , left : String
    }


{-| For now, objects are all rectangular
-}
type alias Object =
    { size : Vector
    , image : Maybe ImageUrls
    , position : Vector
    , velocity : Vector
    , mass : Float
    }
