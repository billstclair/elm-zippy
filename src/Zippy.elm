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
import List.Extra as LE
import Random exposing (Seed)
import Task
import Time exposing (Time)
import Window exposing (Size)
import Zippy.Physics exposing (adjustForCollision)
import Zippy.Render exposing (renderList, renderObject)
import Zippy.SharedTypes
    exposing
        ( ImageChoice
        , ImageUrls
        , Msg(..)
        , Object
        , Rectangle
        , Vector
        , makeRectangle
        , makeSize
        , makeVector
        , rectangleFromVectors
        , sizeToVector
        , zeroRectangle
        , zeroVector
        )


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (\x -> InitialSize x) Window.size


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
    , choices : List ImageChoice
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
    makeVector 160 200


zippy : ImageUrls
zippy =
    { left = "images/zippy-left.jpg"
    , right = "images/zippy-right.jpg"
    }


zippyMass : Float
zippyMass =
    2


milo : ImageUrls
milo =
    { left = "images/milo-head-left.jpg"
    , right = "images/milo-head-right.jpg"
    }


miloMass : Float
miloMass =
    1


zippyChoice : ImageChoice
zippyChoice =
    { image = zippy
    , mass = zippyMass
    , probability = 0.75
    }


miloChoice : ImageChoice
miloChoice =
    { image = milo
    , mass = miloMass
    , probability = 0.25
    }


{-| chooseImage expects probabilities to add to 1.
-}
allChoices : List ImageChoice
allChoices =
    [ zippyChoice
    , miloChoice
    ]


chooseImage : Float -> List ImageChoice -> Maybe ( ImageUrls, Float )
chooseImage x choices =
    let
        loop =
            \x chs ->
                case chs of
                    [] ->
                        case choices of
                            [] ->
                                Nothing

                            ch :: _ ->
                                Just ( ch.image, ch.mass )

                    head :: tail ->
                        if x <= head.probability then
                            Just ( head.image, head.mass )
                        else
                            loop (x - head.probability) tail
    in
    loop x choices


defaultObjectPosition : Vector
defaultObjectPosition =
    makeVector 200 200


defaultObjectRect : Rectangle
defaultObjectRect =
    rectangleFromVectors defaultObjectPosition objectSize


defaultMass : Float
defaultMass =
    1


defaultObject : Object
defaultObject =
    { rect = defaultObjectRect
    , image = Nothing
    , velocity = makeVector 8 4
    , mass = defaultMass
    }


randomFloat : Float -> Float -> Seed -> ( Float, Seed )
randomFloat min max seed =
    Random.step (Random.float min max) seed


randomVector : Vector -> Vector -> Seed -> ( Vector, Seed )
randomVector mins maxs seed =
    let
        ( x, seed2 ) =
            randomFloat mins.x maxs.x seed

        ( y, seed3 ) =
            randomFloat mins.y maxs.y seed
    in
    ( makeVector x y, seed3 )


randomPosition : Size -> Seed -> ( Vector, Seed )
randomPosition size seed =
    let
        maxx =
            toFloat size.width - objectSize.x

        maxy =
            toFloat size.height - objectSize.y
    in
    randomVector zeroVector (makeVector maxx maxy) seed


randomBool : Seed -> ( Bool, Seed )
randomBool seed =
    Random.step Random.bool seed


minVelocity : Vector
minVelocity =
    makeVector 4 4


maxVelocity : Vector
maxVelocity =
    makeVector 12 12


randomVelocity : Seed -> ( Vector, Seed )
randomVelocity seed =
    let
        ( { x, y }, seed2 ) =
            randomVector minVelocity maxVelocity seed

        ( negx, seed3 ) =
            randomBool seed2

        ( negy, seed4 ) =
            randomBool seed3

        xx =
            if negx then
                -x
            else
                x

        yy =
            if negy then
                -y
            else
                y
    in
    ( makeVector xx yy, seed4 )


randomImage : Seed -> List ImageChoice -> ( Maybe ( ImageUrls, Float ), Seed )
randomImage seed choices =
    let
        sum =
            List.foldr (+) 0 (List.map .probability choices)

        ( x, seed2 ) =
            randomFloat 0 sum seed
    in
    ( chooseImage x choices, seed2 )


randomObject : Size -> Seed -> List ImageChoice -> ( Object, Seed )
randomObject size seed choices =
    let
        ( im, seed2 ) =
            randomImage seed choices

        ( img, mass ) =
            case im of
                Nothing ->
                    ( Nothing, defaultMass )

                Just ( im, m ) ->
                    ( Just im, m )

        ( pos, seed3 ) =
            randomPosition size seed2

        ( vel, seed4 ) =
            randomVelocity seed3

        rect =
            defaultObject.rect

        object =
            { defaultObject
                | image = img
                , rect = { rect | position = pos }
                , velocity = vel
                , mass = mass
            }
    in
    ( object, seed4 )


makeInitialObjects : Size -> Seed -> ( List Object, Seed )
makeInitialObjects size seed =
    let
        choices =
            [ zippyChoice ]

        ( o1, seed2 ) =
            randomObject size seed choices

        ( o2, seed3 ) =
            randomObject size seed2 choices

        ( o3, seed4 ) =
            randomObject size seed3 choices
    in
    ( [ o1, o2, o3 ], seed4 )


initialObject : Object
initialObject =
    { rect = makeRectangle 200 200 0 0
    , velocity = makeVector 0 0
    , mass = 1
    , image = Nothing
    }


initialModel : Model
initialModel =
    { windowSize = initialSize
    , seed = Random.initialSeed 0
    , choices = [ zippyChoice ]
    , objects = [ initialObject ]
    , showDialog = False
    , didShow = False
    , running = True
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ initialSizeCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialSize size ->
            { model | windowSize = size }
                ! [ Task.perform Initialize Time.now ]

        Resize size ->
            { model | windowSize = size } ! []

        Initialize time ->
            let
                seed =
                    Random.initialSeed (truncate time)

                ( objects, seed2 ) =
                    makeInitialObjects model.windowSize seed
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
            { model | objects = [] } ! []

        RemoveObject object ->
            --currently ignores object
            case List.reverse model.objects of
                [] ->
                    model ! []

                _ :: tail ->
                    { model | objects = List.reverse tail } ! []

        ToggleChoice choice ->
            let
                choices =
                    [ choice ]
            in
            { model | choices = choices } ! []

        AddObject object ->
            --current ignores object
            let
                ( object, seed ) =
                    randomObject model.windowSize model.seed model.choices

                objects =
                    object :: model.objects
            in
            { model
                | objects = objects
                , seed = seed
            }
                ! []

        SelectObject object ->
            model ! []

        Nop ->
            model ! []


updateObject : Vector -> List Object -> Object -> Object
updateObject windowSize objects object =
    let
        pos =
            object.rect.position

        px =
            pos.x

        py =
            pos.y

        size =
            object.rect.size

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

        rect =
            object.rect
    in
    { object
        | rect = { rect | position = newpos }
        , velocity = newv
    }


processCollisions : List Object -> List Object
processCollisions objects =
    let
        loop : List Object -> List ( Object, Object ) -> List Object -> List Object
        loop =
            \obs done res ->
                case obs of
                    [] ->
                        List.reverse res

                    ob :: tail ->
                        case LE.find (\( o, _ ) -> ob == o) done of
                            Just ( _, oo ) ->
                                loop tail done (oo :: res)

                            Nothing ->
                                case innerLoop ob objects done of
                                    Nothing ->
                                        loop tail done (ob :: res)

                                    Just ( o, pair ) ->
                                        loop tail
                                            (pair :: ( ob, o ) :: done)
                                            (o :: res)

        innerLoop : Object -> List Object -> List ( Object, Object ) -> Maybe ( Object, ( Object, Object ) )
        innerLoop object obs done =
            case obs of
                [] ->
                    Nothing

                ob :: tail ->
                    if ob == object then
                        innerLoop object tail done
                    else
                        case LE.find (\( o, _ ) -> ob == o) done of
                            Just _ ->
                                innerLoop object tail done

                            Nothing ->
                                case adjustForCollision object ob of
                                    Nothing ->
                                        innerLoop object tail done

                                    Just ( objectf, obf ) ->
                                        Just ( objectf, ( ob, obf ) )
    in
    loop objects [] []


updateObjects : Model -> Model
updateObjects model =
    let
        ws =
            sizeToVector model.windowSize

        objects =
            model.objects
                |> processCollisions
                |> List.map (updateObject ws model.objects)
    in
    { model | objects = objects }


btn : String -> Msg -> Html Msg
btn string msg =
    button [ onClick msg ]
        [ text string ]


sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
    img
        [ src url
        , title name
        , alt name
        , width size
        , height size
        ]
        []


logoLink : String -> String -> String -> Int -> Html Msg
logoLink url img name size =
    a
        [ href url
        , target "_blank"
        ]
        [ sqrimg ("images/" ++ img) name size ]


helpLink : String -> String -> Html Msg
helpLink string url =
    a
        [ href url
        , target "_blank"
        ]
        [ text string ]


isZippy : Model -> Bool
isZippy model =
    isChoice zippyChoice model


isMilo : Model -> Bool
isMilo model =
    isChoice miloChoice model


isChoice : ImageChoice -> Model -> Bool
isChoice choice model =
    List.member choice model.choices


choiceCheckbox : String -> ImageChoice -> Model -> Html Msg
choiceCheckbox name choice model =
    checkbox name (isChoice choice model) (ToggleChoice choice)


checkbox : String -> Bool -> msg -> Html msg
checkbox name isChecked msg =
    label []
        [ input
            [ type_ "radio"
            , onClick msg
            , checked isChecked
            ]
            []
        , text name
        ]


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
            [ div [ align "center" ]
                [ div []
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
                , div []
                    [ choiceCheckbox "Zippy" zippyChoice model
                    , text " "
                    , choiceCheckbox "Milo" miloChoice model
                    ]
                , div []
                    [ helpLink "Gib Goy Games" "https://gibgoygames.com/" ]
                , div []
                    [ logoLink "http://elm-lang.org/"
                        "elm-logo-125x125.png"
                        "Elm inside"
                        28
                    , logoLink "https://github.com/billstclair/kakuro-master"
                        "GitHub-Mark-32px.png"
                        "GitHub source code"
                        32
                    ]
                , div []
                    [ btn "Close Dialog" <| ShowDialog False ]
                ]
            ]
        , actionBar =
            []
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
