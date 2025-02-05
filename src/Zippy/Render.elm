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
import Zippy.SharedTypes
    exposing
        ( Msg(..)
        , Object
        , Size
        , rectangleCoordinates
        )
import Zippy.Styles as Styles exposing (SClass(..), classes)


renderObject : Object -> List (Svg Msg)
renderObject object =
    let
        size =
            object.rect.size

        pos =
            object.rect.pos

        box =
            rect
                [ class "SvgCell SvgObjectColor"
                , x <| String.fromFloat pos.x
                , y <| String.fromFloat pos.y
                , width <| String.fromFloat size.x
                , height <| String.fromFloat size.y
                ]
                []

        vx =
            object.velocity.x

        trans =
            if vx < 0 then
                ""

            else
                let
                    ( ( left, _ ), ( right, _ ) ) =
                        rectangleCoordinates object.rect

                    tr =
                        String.fromFloat (left + right)
                in
                "translate(" ++ tr ++ ",0) scale(-1, 1)"

        pict =
            case object.image of
                Nothing ->
                    []

                Just href ->
                    [ image
                        [ xlinkHref href
                        , x <| String.fromFloat (pos.x + 1)
                        , y <| String.fromFloat (pos.y + 1)
                        , width <| String.fromFloat (size.x - 2)
                        , height <| String.fromFloat (size.y - 2)
                        , transform trans
                        ]
                        []
                    ]
    in
    box :: pict


renderList : List Object -> Size -> Html Msg
renderList objects size =
    let
        sw =
            String.fromFloat size.width

        sh =
            String.fromFloat size.height
    in
    div []
        [ Styles.style
        , svg [ width sw, height sh ]
            (rect
                [ class "SvgCell SvgCellColor"
                , x "1"
                , y "1"
                , width <| String.fromFloat (size.width - 2)
                , height <| String.fromFloat (size.height - 2)
                ]
                []
                :: List.concatMap renderObject objects
            )
        ]
