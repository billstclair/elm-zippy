----------------------------------------------------------------------
--
-- Physics.elm
-- Physics for Zippy hack.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Zippy.Physics exposing (adjustForCollision)

import Zippy.SharedTypes
    exposing
        ( Object
        , Rectangle
        , Side(..)
        , Vector
        , makeVector
        , rectangleCoordinates
        , vectorCoordinates
        )


{-| From <https://www.real-world-physics-problems.com/elastic-collision.html>
-}
elasticCollision : Float -> Float -> Float -> Float -> ( Float, Float )
elasticCollision m1 v1 m2 v2 =
    let
        sum =
            m1 + m2

        v1f =
            ((m1 - m2) / sum) * v1 + (2 * m2 / sum) * v2

        v2f =
            (2 * m1 / sum) * v1 + ((m2 - m1) / sum) * v2
    in
    ( v1f, v2f )


objectCollisionSide : Object -> Object -> Maybe Side
objectCollisionSide o1 o2 =
    let
        ( l1, t1, r1, b1 ) =
            rectangleCoordinates o1.rect

        ( l2, t2, r2, b2 ) =
            rectangleCoordinates o2.rect

        ( vx1, vy1 ) =
            vectorCoordinates o1.velocity

        ( vx2, vy2 ) =
            vectorCoordinates o2.velocity

        -- impingements of o1 onto o2
        hOverlaps =
            (l1 > l2 && l1 < r2) || (r1 > l2) && (r1 < r2)

        bottomImpinges =
            hOverlaps && b1 >= t2 && b1 < b2 && vy1 > vy2

        bottomImpinge =
            b1 - t2

        topImpinges =
            hOverlaps && t1 <= b2 && t1 > t2 && vy1 < vy2

        topImpinge =
            b2 - t1

        vOverlaps =
            (b1 > t2 && b1 < b2) || (t1 > t2 && t1 < b2)

        rightImpinges =
            vOverlaps && r1 >= l2 && r1 < r2 && vx1 > vx2

        rightImpinge =
            r1 - l2

        leftImpinges =
            vOverlaps && l1 <= r2 && l1 > l2 && vx1 < vx2

        leftImpinge =
            r2 - l1
    in
    if topImpinges then
        if leftImpinges then
            if rightImpinges then
                if topImpinge <= leftImpinge then
                    if topImpinge <= rightImpinge then
                        Just TopSide
                    else if leftImpinge <= rightImpinge then
                        Just LeftSide
                    else
                        Just RightSide
                else if leftImpinge <= rightImpinge then
                    Just LeftSide
                else
                    Just RightSide
            else if topImpinge <= leftImpinge then
                Just TopSide
            else
                Just LeftSide
        else
            Just TopSide
    else if leftImpinges then
        if rightImpinges then
            if leftImpinge <= rightImpinge then
                Just LeftSide
            else
                Just RightSide
        else
            Just LeftSide
    else if rightImpinges then
        Just RightSide
    else
        Nothing


adjustForCollision : Object -> Object -> Maybe ( Object, Object )
adjustForCollision o1 o2 =
    case objectCollisionSide o1 o2 of
        Nothing ->
            Nothing

        Just side ->
            let
                ( vx1, vy1 ) =
                    vectorCoordinates o1.velocity

                ( vx2, vy2 ) =
                    vectorCoordinates o2.velocity
            in
            if side == LeftSide || side == RightSide then
                let
                    ( vxf1, vxf2 ) =
                        elasticCollision o1.mass vx1 o2.mass vx2
                in
                Just
                    ( { o1 | velocity = makeVector vxf1 vy1 }
                    , { o2 | velocity = makeVector vxf2 vy2 }
                    )
            else
                let
                    ( vyf1, vyf2 ) =
                        elasticCollision o1.mass vy1 o2.mass vy2
                in
                Just
                    ( { o1 | velocity = makeVector vx1 vyf1 }
                    , { o2 | velocity = makeVector vx2 vyf2 }
                    )
