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

import AnimationFrame
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
import Random
import Task
import Time exposing (Time)
import Window exposing (Size)
import Zippy.Render exposing (renderList, renderObject)
import Zippy.SharedTypes
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


objectSize : Vector
objectSize =
    makeVector 200 250


zippy : ImageUrls
zippy =
    { left = "images/zippy-left.jpg"
    , right = "images/zippy-right.jpg"
    }


milo : ImageUrls
milo =
    { left = "images/milo-head-left.jpg"
    , right = "images/milo-head-right.jpg"
    }


type alias ImageChoice =
    { image : ImageUrls
    , probability : Float
    }


choices : List ImageChoice
choices =
    [ { image = zippy
      , probability = 0.75
      }
    , { image = milo
      , probability = 0.25
      }
    ]


chooseImage : Float -> Maybe ImageUrls
chooseImage x =
    let
        loop =
            \x choices ->
                case choices of
                    [] ->
                        Nothing

                    head :: tail ->
                        if x <= head.probability then
                            Just head.image
                        else
                            loop (x - head.probability) tail
    in
    loop x choices


initialObject : Object
initialObject =
    { size = objectSize
    , image = Nothing
    , position = makeVector 200 200
    , velocity = makeVector 8 4
    , mass = 1
    }


initialModel : Model
initialModel =
    { windowSize = initialSize
    , objects = [ initialObject ]
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ resizeCmd
          , Random.generate ChooseImage (Random.float 0 1)
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            { model | windowSize = size } ! []

        Update ->
            updateObjects model ! []

        ChooseImage x ->
            case chooseImage x of
                Nothing ->
                    model ! []

                Just img ->
                    case model.objects of
                        [] ->
                            model ! []

                        object :: tail ->
                            { model | objects = { object | image = Just img } :: tail }
                                ! []

        Nop ->
            model ! []


updateObject : Vector -> List Object -> Object -> Object
updateObject windowSize objects object =
    let
        pos =
            object.position

        px =
            pos.x

        py =
            pos.y

        size =
            object.size

        sx =
            size.x

        sy =
            size.y

        right =
            px + sx

        bottom =
            py + sy

        w =
            windowSize.x

        h =
            windowSize.y

        v =
            object.velocity

        vx =
            v.x

        vy =
            v.y

        newpos =
            { x = px + vx
            , y = py + vy
            }

        newvx =
            if vx > 0 && right >= w then
                -vx
            else if vx < 0 && px <= 0 then
                -vx
            else
                vx

        newvy =
            if vy > 0 && bottom >= h then
                -vy
            else if vy < 0 && py <= 0 then
                -vy
            else
                vy

        newv =
            { x = newvx, y = newvy }
    in
    { object | position = newpos, velocity = newv }


updateObjects : Model -> Model
updateObjects model =
    let
        ws =
            sizeToVector model.windowSize

        objects =
            List.map
                (updateObject ws model.objects)
                model.objects
    in
    { model | objects = objects }


view : Model -> Html Msg
view model =
    renderList model.objects model.windowSize


refreshPeriod : Time
refreshPeriod =
    20 * Time.millisecond


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.times (\_ -> Update)
        ]
