----------------------------------------------------------------------
--
-- Render.elm
-- Code to draw the objects
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Zippy.Render exposing (renderList, renderObject)

import Html exposing (Html, div, text)
import Svg exposing (Svg, g, image, line, rect, svg)
import Svg.Attributes
    exposing
        ( class
        , fill
        , fontSize
        , height
        , stroke
        , transform
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )
import Window exposing (Size)
import Zippy.SharedTypes
    exposing
        ( Msg(..)
        , Object
        , Vector
        , makeSize
        , makeVector
        , zeroVector
        )
import Zippy.Styles as Styles exposing (SClass(..), classes)


renderObject : Object -> List (Svg Msg)
renderObject object =
    let
        size =
            object.size

        pos =
            object.position

        box =
            rect
                [ class "SvgCell SvgObjectColor"
                , x <| toString pos.x
                , y <| toString pos.y
                , width <| toString size.x
                , height <| toString size.y
                ]
                []

        vx =
            object.velocity.x

        leftImage =
            object.image

        rightImage =
            object.rightImage

        img =
            if vx < 0 then
                leftImage
            else if rightImage == Nothing then
                leftImage
            else
                rightImage

        pict =
            case img of
                Nothing ->
                    []

                Just href ->
                    [ image
                        [ xlinkHref href
                        , x <| toString (pos.x + 1)
                        , y <| toString (pos.y + 1)
                        , width <| toString (size.x - 2)
                        , height <| toString (size.y - 2)
                        ]
                        []
                    ]
    in
    box :: pict


renderList : List Object -> Size -> Html Msg
renderList objects size =
    let
        sw =
            toString size.width

        sh =
            toString size.height
    in
    div []
        [ Styles.style
        , svg [ width sw, height sh ]
            (rect
                [ class "SvgCell SvgCellColor"
                , x "1"
                , y "1"
                , width <| toString (size.width - 2)
                , height <| toString (size.height - 2)
                ]
                []
                :: List.concatMap renderObject objects
            )
        ]
