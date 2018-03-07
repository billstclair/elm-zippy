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
import Dialog
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
import Random exposing (Seed)
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
    , seed : Seed
    , objects : List Object
    , showDialog : Bool
    , didShow : Bool
    , running : Bool
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


{-| chooseImage expects probabilities to add to 1.
-}
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


defaultObject : Object
defaultObject =
    { size = objectSize
    , image = Nothing
    , position = makeVector 200 200
    , velocity = makeVector 8 4
    , mass = 1
    }


makeInitialObjects : Seed -> ( List Object, Seed )
makeInitialObjects seed =
    let
        ( x, seed2 ) =
            Random.step (Random.float 0 1) seed
    in
    case chooseImage x of
        Nothing ->
            ( [ defaultObject ], seed2 )

        maybeImg ->
            ( [ { defaultObject | image = maybeImg } ], seed2 )


initialObject : Object
initialObject =
    { size = objectSize
    , image = Nothing
    , position = makeVector 200 200
    , velocity = makeVector 0 0
    , mass = 1
    }


initialModel : Model
initialModel =
    { windowSize = initialSize
    , seed = Random.initialSeed 0
    , objects = [ initialObject ]
    , showDialog = True
    , didShow = False
    , running = True
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ resizeCmd
          , Task.perform Initialize Time.now
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            { model | windowSize = size } ! []

        Initialize time ->
            let
                seed =
                    Random.initialSeed (truncate time)

                ( objects, seed2 ) =
                    makeInitialObjects seed
            in
            { model
                | seed = seed2
                , objects = objects
            }
                ! []

        Update ->
            if model.running then
                updateObjects model ! []
            else
                model ! []

        ShowDialog show ->
            { model
                | showDialog = show
                , didShow = model.didShow || show
            }
                ! []

        Run run ->
            { model | running = run } ! []

        Clear ->
            model ! []

        RemoveObject object ->
            model ! []

        AddObject object ->
            model ! []

        SelectObject object ->
            model ! []

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


btn : String -> Msg -> Html Msg
btn string msg =
    button [ onClick msg ]
        [ text string ]


dialog : Model -> Html Msg
dialog model =
    let
        run =
            not model.running
    in
    Dialog.render
        { styles = []
        , title = ""
        , content =
            [ btn "Clear" Clear
            , btn "Add" <| AddObject initialObject
            , btn "Remove" <| RemoveObject initialObject
            , btn
                (if run then
                    "Run"
                 else
                    "Stop"
                )
                (Run run)
            ]
        , actionBar =
            [ btn "Close Dialog" <| ShowDialog False ]
        }
        True


view : Model -> Html Msg
view model =
    div []
        [ if model.showDialog then
            dialog model
          else
            button
                [ onClick <| ShowDialog True
                , style
                    [ ( "position", "fixed" )
                    , ( "top", "10px" )
                    , ( "left", "10px" )
                    ]
                ]
                [ text <|
                    if model.didShow then
                        "+"
                    else
                        "Click Me!"
                ]
        , renderList model.objects model.windowSize
        ]


refreshPeriod : Time
refreshPeriod =
    20 * Time.millisecond


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.times (\_ -> Update)
        ]
