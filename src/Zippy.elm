----------------------------------------------------------------------
--
-- Zippy.elm
-- Zippy the Pinhead careening around the screen.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Zippy exposing (..)

import Debug exposing (log)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , button
        , div
        , fieldset
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , p
        , span
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( align
        , alt
        , checked
        , colspan
        , disabled
        , height
        , href
        , name
        , placeholder
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import SharedTypes
    exposing
        ( Msg(..)
        , Object
        , Vector
        , makeSize
        , makeVector
        , zeroVector
        )
import Task
import Window exposing (Size)


resizeCmd : Cmd Msg
resizeCmd =
    Task.perform (\x -> Resize x) Window.size


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { windowSize : Size
    , objects : List Object
    }


initialSize : Size
initialSize =
    { width = 500
    , height = 500
    }


initialObject : Object
initialObject =
    { size = makeSize 50 75
    , image = Nothing
    , position = makeVector 200 200
    , velocity = makeVector 10 10
    , mass = 1
    }


initialModel : Model
initialModel =
    { windowSize = initialSize
    , objects = [ initialObject ]
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, resizeCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text "Hello World!"


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes Resize
