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

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
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
import Html.Lazy as Lazy
import Json.Decode as JD exposing (Decoder)
import List.Extra as LE
import Random exposing (Seed)
import Task
import Time exposing (Posix)
import Zippy.Physics exposing (adjustForCollision)
import Zippy.Render exposing (renderList, renderObject)
import Zippy.SharedTypes
    exposing
        ( ImageChoice
        , ImageUrl
        , Msg(..)
        , Object
        , ObjectSounds
        , Position
        , Rectangle
        , Size
        , SoundType(..)
        , Vector
        , distanceToRectangle
        , isVectorInRectangle
        , makeRectangle
        , makeSize
        , makeVector
        , objectSoundLeft
        , objectSoundRight
        , positionToVector
        , rectangleCenter
        , rectangleFromVectors
        , scaleRectangle
        , scaleVector
        , sizeToVector
        , vectorDifference
        , vectorSum
        , zeroRectangle
        , zeroVector
        )
import Zippy.Sound as Sound


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (\x -> InitialSize x) Dom.getViewport


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { windowSize : Size
    , seed : Seed
    , soundType : SoundType
    , choices : List ImageChoice
    , objects : List Object
    , scale : Float
    , showDialog : Bool
    , didShow : Bool
    , running : Bool
    , index : Int
    , grabbedIndex : Int
    , grabbedOffset : Vector
    , grabbedPos : Vector
    }


nextIndex : Model -> ( Int, Model )
nextIndex model =
    let
        index =
            model.index + 1
    in
    ( index
    , { model | index = index }
    )


setObjectIndex : Object -> Model -> ( Object, Model )
setObjectIndex object model =
    let
        ( index, mdl ) =
            nextIndex model
    in
    ( { object | index = index }
    , mdl
    )


initialSize : Size
initialSize =
    { width = 500
    , height = 500
    }


objectSize : Vector
objectSize =
    makeVector 160 200


zippy : ImageUrl
zippy =
    "images/zippy-left.jpg"


zippyMass : Float
zippyMass =
    2


milo : ImageUrl
milo =
    "images/milo-cartoon-left.jpg"


miloMass : Float
miloMass =
    1


mrNatural : ImageUrl
mrNatural =
    "images/mr-natural-left.jpg"


mrNaturalMass : Float
mrNaturalMass =
    3


zippyChoice : ImageChoice
zippyChoice =
    { image = zippy
    , sounds = ( "zippy-left", "zippy-right" )
    , mass = zippyMass
    , probability = 0.75
    }


miloChoice : ImageChoice
miloChoice =
    { image = milo
    , sounds = ( "milo-left", "milo-right" )
    , mass = miloMass
    , probability = 0.25
    }


mrNaturalChoice : ImageChoice
mrNaturalChoice =
    { image = mrNatural
    , sounds = ( "mr-natural-left", "mr-natural-right" )
    , mass = mrNaturalMass
    , probability = 0.25
    }


{-| chooseImage expects probabilities to add to 1.
-}
allChoices : List ImageChoice
allChoices =
    [ zippyChoice
    , miloChoice
    , mrNaturalChoice
    ]


chooseImage : Float -> List ImageChoice -> Maybe ( ImageUrl, Float, ObjectSounds )
chooseImage x choices =
    let
        loop xx chs =
            case chs of
                [] ->
                    case choices of
                        [] ->
                            Nothing

                        ch :: _ ->
                            Just ( ch.image, ch.mass, ch.sounds )

                head :: tail ->
                    if xx <= head.probability then
                        Just ( head.image, head.mass, head.sounds )

                    else
                        loop (xx - head.probability) tail
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
    { index = -1
    , sounds = ( "", "" )
    , rect = defaultObjectRect
    , image = Nothing
    , velocity = makeVector 8 4
    , mass = defaultMass
    , sticky = False
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


randomPosition : Float -> Size -> Seed -> ( Vector, Seed )
randomPosition scale size seed =
    let
        maxx =
            size.width - scale * objectSize.x

        maxy =
            size.height - scale * objectSize.y
    in
    randomVector zeroVector (makeVector maxx maxy) seed


randomBool : Seed -> ( Bool, Seed )
randomBool seed =
    let
        ( int, newSeed ) =
            Random.step (Random.int 0 1) seed

        bool =
            if int == 0 then
                False

            else
                True
    in
    ( bool, newSeed )


minVelocity : Vector
minVelocity =
    makeVector 4 4


maxVelocity : Vector
maxVelocity =
    makeVector 12 12


randomVelocity : Float -> Seed -> ( Vector, Seed )
randomVelocity scale seed =
    let
        ( { x, y }, seed2 ) =
            randomVector
                (scaleVector scale minVelocity)
                (scaleVector scale maxVelocity)
                seed

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


randomImage : Seed -> List ImageChoice -> ( Maybe ( ImageUrl, Float, ObjectSounds ), Seed )
randomImage seed choices =
    let
        sum =
            List.foldr (+) 0 (List.map .probability choices)

        ( x, seed2 ) =
            randomFloat 0 sum seed
    in
    ( chooseImage x choices, seed2 )


randomObject : Float -> Size -> Seed -> List ImageChoice -> ( Object, Seed )
randomObject scale size seed choices =
    let
        ( im, seed2 ) =
            randomImage seed choices

        ( img, mass, sounds ) =
            case im of
                Nothing ->
                    ( Nothing, defaultMass, ( "", "" ) )

                Just ( i, m, s ) ->
                    ( Just i, m, s )

        ( pos, seed3 ) =
            randomPosition scale size seed2

        ( vel, seed4 ) =
            randomVelocity scale seed3

        rect =
            defaultObject.rect
                |> scaleRectangle scale

        object =
            { defaultObject
                | image = img
                , sounds = sounds
                , rect = { rect | pos = pos }
                , velocity = vel
                , mass = mass
            }
    in
    ( object, seed4 )


initialObjectCount : Int
initialObjectCount =
    3


addRandomObject : List ImageChoice -> Model -> Model
addRandomObject choices model =
    let
        ( object, seed ) =
            randomObject model.scale model.windowSize model.seed choices

        ( ob, mdl ) =
            setObjectIndex object model
    in
    { mdl
        | objects = mdl.objects ++ [ ob ]
        , seed = seed
    }


makeInitialObjects : Model -> Model
makeInitialObjects model =
    let
        size =
            model.windowSize

        seed =
            model.seed

        choices =
            [ zippyChoice, mrNaturalChoice, miloChoice, zippyChoice ]

        loop : List ImageChoice -> Model -> Model
        loop ch mdl =
            case ch of
                [] ->
                    mdl

                choice :: tail ->
                    loop tail <| addRandomObject [ choice ] mdl
    in
    loop choices model


initialModel : Model
initialModel =
    { windowSize = initialSize
    , seed = Random.initialSeed 0
    , soundType = NoSound
    , choices = [ zippyChoice ]
    , objects = []
    , scale = 1.0
    , showDialog = False
    , didShow = False
    , running = True
    , index = -1
    , grabbedIndex = -1
    , grabbedOffset = zeroVector
    , grabbedPos = zeroVector
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch [ initialSizeCmd, Sound.registerSounds ]
    )


objectsPerDimension : Float
objectsPerDimension =
    5


computeScale : Size -> Float
computeScale { width, height } =
    let
        scaleDimension dim size =
            dim / objectsPerDimension / size

        scalex =
            scaleDimension width objectSize.x

        scaley =
            scaleDimension height objectSize.y
    in
    min 1 (min scalex scaley)


rescaleObject : Float -> Float -> Object -> Object
rescaleObject oldScale newScale object =
    let
        pos =
            object.rect.pos

        rect =
            defaultObject.rect
                |> scaleRectangle newScale
    in
    { object
        | rect = { rect | pos = pos }
        , velocity = scaleVector (newScale / oldScale) object.velocity
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialSize { viewport } ->
            let
                windowSize =
                    makeSize viewport.width viewport.height

                scale =
                    computeScale windowSize
            in
            ( { model
                | windowSize = windowSize
                , scale = scale
              }
            , Task.perform Initialize Time.now
            )

        Resize size ->
            let
                newScale =
                    computeScale size

                newObjects =
                    if newScale == model.scale then
                        model.objects

                    else
                        List.map (rescaleObject model.scale newScale) model.objects
            in
            ( { model
                | windowSize = size
                , scale = newScale
                , objects = newObjects
              }
            , Cmd.none
            )

        Initialize posix ->
            let
                seed =
                    Random.initialSeed (Time.posixToMillis posix)

                mdl =
                    makeInitialObjects
                        { model | seed = seed }
            in
            ( mdl
            , Cmd.none
            )

        Update ->
            if not model.running then
                ( model
                , Cmd.none
                )

            else
                let
                    index =
                        model.grabbedIndex

                    grabbed =
                        LE.find (\ob -> ob.index == index) model.objects

                    ( mdl, cmd ) =
                        updateObjects model
                in
                case grabbed of
                    Nothing ->
                        ( mdl
                        , cmd
                        )

                    Just go ->
                        let
                            updater =
                                \ob ->
                                    let
                                        rect =
                                            ob.rect
                                    in
                                    { ob
                                        | rect = { rect | pos = go.rect.pos }
                                        , velocity = zeroVector
                                    }

                            ( mob, mdl2 ) =
                                updateObjectAtIndex updater index mdl
                        in
                        case mob of
                            Nothing ->
                                ( mdl2
                                , cmd
                                )

                            Just ob ->
                                let
                                    pos =
                                        mdl2.grabbedPos

                                    newpos =
                                        ob.rect.pos

                                    vel =
                                        vectorDifference newpos pos

                                    ( _, mdl3 ) =
                                        updateObjectAtIndex
                                            (\obj -> { obj | velocity = vel })
                                            ob.index
                                            mdl2
                                in
                                ( { mdl3 | grabbedPos = newpos }
                                , cmd
                                )

        ShowDialog show ->
            ( { model
                | showDialog = show
                , didShow = model.didShow || show
              }
            , Cmd.none
            )

        Run run ->
            ( { model | running = run }
            , Cmd.none
            )

        Clear ->
            ( { model | objects = [] }
            , Cmd.none
            )

        RemoveObject ->
            case List.reverse model.objects of
                [] ->
                    ( model
                    , Cmd.none
                    )

                _ :: tail ->
                    ( { model | objects = List.reverse tail }
                    , Cmd.none
                    )

        ToggleChoice choice ->
            let
                choices =
                    [ choice ]
            in
            ( { model | choices = choices }
            , Cmd.none
            )

        SetSoundType soundType ->
            ( { model | soundType = soundType }
            , Cmd.none
            )

        AddObject ->
            ( addRandomObject model.choices model
            , Cmd.none
            )

        SelectObject object ->
            ( model
            , Cmd.none
            )

        MouseDown pos ->
            if model.showDialog || pos.y < 50 then
                ( model
                , Cmd.none
                )

            else
                let
                    vect =
                        positionToVector pos
                in
                case findClosestObject vect model of
                    Nothing ->
                        ( model
                        , Cmd.none
                        )

                    Just ob ->
                        let
                            mdl =
                                { model
                                    | grabbedIndex = ob.index
                                    , grabbedOffset =
                                        vectorDifference
                                            ob.rect.pos
                                        <|
                                            positionToVector pos
                                }
                        in
                        ( mdl, Cmd.none )

        MouseUp pos ->
            let
                ( vel, seed ) =
                    randomVelocity model.scale model.seed

                mdl2 =
                    { model | seed = seed }

                updater =
                    \ob ->
                        if ob.velocity == zeroVector then
                            { ob | velocity = vel }

                        else
                            ob

                ( _, mdl3 ) =
                    updateObjectAtIndex updater model.grabbedIndex mdl2
            in
            ( { mdl3 | grabbedIndex = -1 }
            , Cmd.none
            )

        MouseMove pos ->
            let
                index =
                    model.grabbedIndex

                offset =
                    model.grabbedOffset

                vect =
                    positionToVector pos

                updater =
                    \ob ->
                        let
                            rect =
                                ob.rect
                        in
                        { ob
                            | rect = { rect | pos = vectorSum vect offset }
                        }
            in
            if index < 0 then
                ( model
                , Cmd.none
                )

            else
                let
                    ( _, mdl ) =
                        updateObjectAtIndex updater index model
                in
                ( mdl
                , Cmd.none
                )

        Nop ->
            ( model
            , Cmd.none
            )


findClosestObject : Vector -> Model -> Maybe Object
findClosestObject vect model =
    List.foldr
        (\ob ( o, min ) ->
            let
                d =
                    distanceToRectangle vect ob.rect
            in
            if d < min then
                ( Just ob, d )

            else
                ( o, min )
        )
        ( Nothing, 1000000 )
        model.objects
        |> Tuple.first


updateObjectAtIndex : (Object -> Object) -> Int -> Model -> ( Maybe Object, Model )
updateObjectAtIndex f index model =
    let
        loop : List Object -> List Object -> ( Maybe Object, Model )
        loop objects res =
            case objects of
                [] ->
                    ( Nothing, model )

                ob :: tail ->
                    if index /= ob.index then
                        loop tail (ob :: res)

                    else
                        let
                            ob2 =
                                f ob
                        in
                        ( Just ob2
                        , { model
                            | objects =
                                List.concat
                                    [ List.reverse res, ob2 :: tail ]
                          }
                        )
    in
    loop model.objects []


updateObject : Vector -> List Object -> Object -> Object
updateObject windowSize objects object =
    let
        pos =
            object.rect.pos

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
            if vx >= 0 && right >= w then
                if vx == 0 then
                    -1

                else
                    -vx

            else if vx <= 0 && px <= 0 then
                if vx == 0 then
                    1

                else
                    -vx

            else
                vx

        newvy =
            if vy >= 0 && bottom >= h then
                if vy == 0 then
                    -1

                else
                    -vy

            else if vy <= 0 && py <= 0 then
                if vy == 0 then
                    1

                else
                    -vy

            else
                vy

        newv =
            { x = newvx, y = newvy }

        rect =
            object.rect
    in
    { object
        | rect = { rect | pos = newpos }
        , velocity = newv
    }


processCollisions : List Object -> List Object
processCollisions objects =
    let
        loop : List Object -> List ( Object, Object ) -> List Object -> List Object
        loop obs done res =
            case obs of
                [] ->
                    List.reverse res

                ob :: tail ->
                    case LE.find (\( o, _ ) -> ob == o) done of
                        Just ( _, oo ) ->
                            loop tail done (oo :: res)

                        Nothing ->
                            case innerLoop ob tail done of
                                Nothing ->
                                    loop tail done (ob :: res)

                                Just ( o, pair ) ->
                                    loop tail (pair :: done) (o :: res)

        innerLoop : Object -> List Object -> List ( Object, Object ) -> Maybe ( Object, ( Object, Object ) )
        innerLoop object obs done =
            case obs of
                [] ->
                    Nothing

                ob :: tail ->
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


updateObjects : Model -> ( Model, Cmd msg )
updateObjects model =
    let
        ws =
            sizeToVector model.windowSize

        objects =
            model.objects
                |> processCollisions
                |> List.map (updateObject ws model.objects)

        soundCmds =
            doSounds model.windowSize objects model.objects model
    in
    ( { model | objects = objects }, Cmd.batch soundCmds )


doSounds : Size -> List Object -> List Object -> Model -> List (Cmd msg)
doSounds windowSize updated original model =
    let
        soundType =
            model.soundType

        useHorizontal =
            (soundType == SoundOn)
                || ((soundType == OneDimSound)
                        && (windowSize.width >= windowSize.height)
                   )

        useVertical =
            (soundType == SoundOn)
                || ((soundType == OneDimSound)
                        && (windowSize.width < windowSize.height)
                   )

        folder : Object -> List (Cmd msg) -> List (Cmd msg)
        folder up cmds =
            case LE.find (\o -> up.index == o.index) original of
                Nothing ->
                    cmds

                Just o ->
                    if up.velocity == o.velocity then
                        cmds

                    else if
                        (useHorizontal && up.velocity.x < o.velocity.x)
                            || (useVertical && up.velocity.y < o.velocity.y)
                    then
                        objectSound o 1 :: cmds

                    else if
                        (useHorizontal && up.velocity.x > o.velocity.x)
                            || (useVertical && up.velocity.y > o.velocity.y)
                    then
                        objectSound o -1 :: cmds

                    else
                        cmds
    in
    List.foldr folder [] updated


objectSound : Object -> Int -> Cmd msg
objectSound o d =
    (if d < 0 then
        objectSoundRight o.sounds

     else
        objectSoundLeft o.sounds
    )
        |> Sound.playSound


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


isMrNatural : Model -> Bool
isMrNatural model =
    isChoice mrNaturalChoice model


isChoice : ImageChoice -> Model -> Bool
isChoice choice model =
    List.member choice model.choices


choiceRadioButton : String -> ImageChoice -> Model -> Html Msg
choiceRadioButton name choice model =
    radioButton name (isChoice choice model) (ToggleChoice choice)


radioButtonOfType : String -> String -> Bool -> msg -> Html msg
radioButtonOfType theType rbn isChecked msg =
    label []
        [ input
            [ type_ "radio"
            , name theType
            , onClick msg
            , checked isChecked
            ]
            []
        , text rbn
        ]


radioButton : String -> Bool -> msg -> Html msg
radioButton =
    radioButtonOfType "radio"


checkbox : String -> Bool -> msg -> Html msg
checkbox name isChecked msg =
    label []
        [ input
            [ type_ "checkbox"
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

        soundType =
            model.soundType
    in
    Dialog.render
        { styles = []
        , title = ""
        , content =
            [ div [ align "center" ]
                [ div []
                    [ btn "Add" <| AddObject
                    , text " "
                    , btn "Remove" <| RemoveObject
                    , text " "
                    , btn "Clear" Clear
                    , text " "
                    , btn
                        (if run then
                            "Run"

                         else
                            "Stop"
                        )
                        (Run run)
                    ]
                , p []
                    [ choiceRadioButton "Zippy" zippyChoice model
                    , text " "
                    , choiceRadioButton "Mr. Natural" mrNaturalChoice model
                    , text " "
                    , choiceRadioButton "Milo" miloChoice model
                    ]
                , p []
                    [ radioButtonOfType "sound" "NoSound" (soundType == NoSound) (SetSoundType NoSound)
                    , text " "
                    , radioButtonOfType "sound" "SoundOn" (soundType == SoundOn) (SetSoundType SoundOn)
                    , text " "
                    , radioButtonOfType "sound" "OneDimSound" (soundType == OneDimSound) (SetSoundType OneDimSound)
                    ]
                , div []
                    [ lines
                        [ "Choose a character and click 'Add' to add it."
                        , "Click 'Remove' to remove the least-recently added character."
                        , "Click 'Clear' to remove all characters."
                        , "Click 'Stop' to stop animation, and 'Run' to restart it."
                        , ""
                        , "Check 'Make Sounds' to make sounds. Annoying. Fun?"
                        , ""
                        , "Click on the board to pick up the nearest character."
                        , "Move the mouse to drag it around."
                        , "If you let the mouse up while stationary, it will get a random velocity."
                        , ""
                        , "Click 'Close Dialog' below to get rid of this."
                        ]
                    ]
                , p []
                    [ helpLink "Gib Goy Games" "https://gibgoygames.com/" ]
                , div []
                    [ logoLink "http://elm-lang.org/"
                        "elm-logo-125x125.png"
                        "Elm inside"
                        28
                    , text " "
                    , logoLink "https://github.com/billstclair/elm-zippy"
                        "GitHub-Mark-32px.png"
                        "GitHub source code"
                        32
                    ]
                , p []
                    [ btn "Close Dialog" <| ShowDialog False ]
                ]
            ]
        , actionBar =
            []
        }
        True


br : Html msg
br =
    Html.br [] []


lines : List String -> Html Msg
lines strings =
    p [] (List.concatMap (\s -> [ text s, br ]) strings)


view : Model -> Html Msg
view model =
    div []
        [ if model.showDialog then
            dialog model

          else
            button
                [ onClick <| ShowDialog True
                , style "position" "fixed"
                , style "top" "10px"
                , style "left" "10px"
                ]
                [ text <|
                    if model.didShow then
                        "+"

                    else
                        "Click Me!"
                ]
        , Lazy.lazy2 renderList model.objects model.windowSize
        ]


mouseDecoder : (Position -> Msg) -> Decoder Msg
mouseDecoder position =
    JD.field "screenX" JD.int
        |> JD.andThen
            (\screenX ->
                JD.field "screenY" JD.int
                    |> JD.andThen
                        (\screenY ->
                            JD.succeed (position { x = screenX, y = screenY })
                        )
            )


{-| Need to integrate
<http://package.elm-lang.org/packages/knledg/touch-events/latest>
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize
            (\w h -> Resize { width = toFloat w, height = toFloat h })
        , if model.running then
            Events.onAnimationFrame (\_ -> Update)

          else
            Sub.none
        , Events.onMouseDown (mouseDecoder MouseDown)
        , Events.onMouseUp (mouseDecoder MouseUp)
        , if model.grabbedIndex >= 0 then
            Events.onMouseMove (mouseDecoder MouseMove)

          else
            Sub.none
        ]
