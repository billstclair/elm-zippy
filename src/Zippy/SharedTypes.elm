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


module Zippy.SharedTypes exposing
    ( Direction(..)
    , ImageChoice
    , ImageUrl
    , Msg(..)
    , Object
    , ObjectSounds
    , Position
    , Rectangle
    , Size
    , SoundType(..)
    , Vector
    , combineVectors
    , distanceToRectangle
    , isVectorInRectangle
    , makeRectangle
    , makeSize
    , makeVector
    , objectSoundLeft
    , objectSoundRight
    , positionToVector
    , rectangleCenter
    , rectangleCoordinates
    , rectangleFromVectors
    , scaleRectangle
    , scaleVector
    , sizeToVector
    , vectorCoordinates
    , vectorDifference
    , vectorDistance
    , vectorSum
    , zeroRectangle
    , zeroVector
    )

import Browser.Dom as Dom exposing (Viewport)
import Time exposing (Posix)


type alias Position =
    { x : Int
    , y : Int
    }


type alias Size =
    { width : Float
    , height : Float
    }


type Msg
    = InitialSize Viewport
    | Resize Size
    | Initialize Posix
    | Update
    | ShowDialog Bool
    | Run Bool
    | Clear
    | RemoveObject
    | AddObject
    | ToggleChoice ImageChoice
    | SetSoundType SoundType
    | SelectObject Object
    | MouseDown Position
    | MouseUp Position
    | MouseMove Position
    | Nop


{-| The type of sounds. Set by the chooser in the pop-up dialog.
-}
type SoundType
    = NoSound
    | SoundOn
    | OneDimSound


type alias ObjectSounds =
    ( String, String )


objectSoundLeft : ObjectSounds -> String
objectSoundLeft sounds =
    Tuple.first sounds


objectSoundRight : ObjectSounds -> String
objectSoundRight sounds =
    Tuple.second sounds


type alias ImageChoice =
    { image : ImageUrl
    , sounds : ObjectSounds
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
    makeVector size.width size.height


positionToVector : Position -> Vector
positionToVector pos =
    makeVector (toFloat pos.x) (toFloat pos.y)


zeroVector : Vector
zeroVector =
    makeVector 0 0


combineVectors : (Float -> Float -> Float) -> Vector -> Vector -> Vector
combineVectors f v1 v2 =
    makeVector (f v1.x v2.x) (f v1.y v2.y)


vectorSum =
    combineVectors (+)


vectorDifference =
    combineVectors (-)


vectorDistance : Vector -> Vector -> Float
vectorDistance v1 v2 =
    let
        diff =
            vectorDifference v1 v2
    in
    sqrt (diff.x ^ 2 + diff.y ^ 2)


scaleVector : Float -> Vector -> Vector
scaleVector scale vector =
    { x = scale * vector.x
    , y = scale * vector.y
    }


type alias Rectangle =
    { pos : Vector --top-left corner
    , size : Vector
    }


{-| Result is (left, top, right bottom)
-}
rectangleCoordinates : Rectangle -> ( ( Float, Float ), ( Float, Float ) )
rectangleCoordinates rect =
    let
        pos =
            rect.pos

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
    ( ( left, top ), ( right, bottom ) )


makeRectangle : Float -> Float -> Float -> Float -> Rectangle
makeRectangle left top width height =
    rectangleFromVectors
        (makeVector left top)
        (makeVector width height)


rectangleFromVectors : Vector -> Vector -> Rectangle
rectangleFromVectors pos size =
    { pos = pos
    , size = size
    }


zeroRectangle : Rectangle
zeroRectangle =
    rectangleFromVectors zeroVector zeroVector


scaleRectangle : Float -> Rectangle -> Rectangle
scaleRectangle scale rect =
    { rect | size = scaleVector scale rect.size }


rectangleCenter : Rectangle -> Vector
rectangleCenter rect =
    let
        ( ( left, top ), ( right, bottom ) ) =
            rectangleCoordinates rect
    in
    makeVector ((left + right) / 2) ((top + bottom) / 2)


distanceToRectangle : Vector -> Rectangle -> Float
distanceToRectangle vect rect =
    let
        center =
            rectangleCenter rect
    in
    vectorDistance vect center


isVectorInRectangle : Vector -> Rectangle -> Bool
isVectorInRectangle vect rect =
    let
        ( ( left, top ), ( right, bottom ) ) =
            rectangleCoordinates rect

        ( x, y ) =
            vectorCoordinates vect
    in
    x >= left && x <= right && y >= top && y <= bottom


makeSize : Float -> Float -> Size
makeSize w h =
    { width = w, height = h }


type alias ImageUrl =
    String


{-| For now, objects are all rectangular
-}
type alias Object =
    { index : Int
    , sounds : ( String, String )
    , rect : Rectangle
    , velocity : Vector
    , mass : Float
    , sticky : Bool
    , image : Maybe ImageUrl
    }
