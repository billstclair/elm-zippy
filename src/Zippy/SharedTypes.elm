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
        ( Direction(..)
        , ImageChoice
        , ImageUrls
        , Msg(..)
        , Object
        , Rectangle
        , Vector
        , makeRectangle
        , makeSize
        , makeVector
        , rectangleCoordinates
        , rectangleFromVectors
        , sizeToVector
        , vectorCoordinates
        , zeroRectangle
        , zeroVector
        )

import Time exposing (Time)
import Window exposing (Size)


type Msg
    = InitialSize Size
    | Resize Size
    | Initialize Time
    | Update
    | ShowDialog Bool
    | Run Bool
    | Clear
    | RemoveObject Object
    | AddObject Object
    | ToggleChoice ImageChoice
    | SelectObject Object
    | Nop


type alias ImageChoice =
    { image : ImageUrls
    , mass : Float
    , probability : Float
    }


type Direction
    = Horizontal
    | Vertical


type alias Vector =
    { x : Float
    , y : Float
    }


makeVector : Float -> Float -> Vector
makeVector x y =
    { x = x, y = y }


vectorCoordinates : Vector -> ( Float, Float )
vectorCoordinates vector =
    ( vector.x, vector.y )


sizeToVector : Size -> Vector
sizeToVector size =
    { x = toFloat size.width
    , y = toFloat size.height
    }


zeroVector : Vector
zeroVector =
    makeVector 0 0


type alias Rectangle =
    { position : Vector --top-left corner
    , size : Vector
    }


{-| Result is (left, top, right bottom)
-}
rectangleCoordinates : Rectangle -> ( Float, Float, Float, Float )
rectangleCoordinates rect =
    let
        pos =
            rect.position

        size =
            rect.size

        left =
            pos.x

        top =
            pos.y

        right =
            left + size.x

        bottom =
            top + size.y
    in
    ( left, top, right, bottom )


makeRectangle : Float -> Float -> Float -> Float -> Rectangle
makeRectangle left top width height =
    rectangleFromVectors
        (makeVector left top)
        (makeVector width height)


rectangleFromVectors : Vector -> Vector -> Rectangle
rectangleFromVectors position size =
    { position = position
    , size = size
    }


zeroRectangle : Rectangle
zeroRectangle =
    rectangleFromVectors zeroVector zeroVector


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
    { rect : Rectangle
    , velocity : Vector
    , mass : Float
    , sticky : Bool
    , image : Maybe ImageUrls
    }
