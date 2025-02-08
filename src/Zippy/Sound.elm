----------------------------------------------------------------------
--
-- Sound.elm
-- Make sounds.
-- Copyright (c) 2025 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module Zippy.Sound exposing (playSound, registerSounds)

import Json.Encode as JE exposing (Value)


type alias SoundCmd =
    { cmd : String
    , file : String
    , idx : String
    }


sounds : List String
sounds =
    [ "milo-left"
    , "milo-right"
    , "mr-natural-left"
    , "mr-natural-right"
    , "zippy-left"
    , "zippy-right"
    ]


soundToUrl : String -> String
soundToUrl sound =
    "sounds/" ++ sound ++ ".mp3"


encodeSoundCmd : SoundCmd -> Value
encodeSoundCmd { cmd, file, idx } =
    JE.object
        [ ( "cmd", JE.string cmd )
        , ( "file", JE.string file )
        , ( "idx", JE.string idx )
        ]


makeSoundCmd : String -> String -> SoundCmd
makeSoundCmd cmd sound =
    { cmd = cmd
    , file = soundToUrl sound
    , idx = sound
    }


registerSound : String -> Cmd msg
registerSound sound =
    makeSoundCmd "create" sound
        |> sendSoundCmd


registerSounds : Cmd msg
registerSounds =
    List.map registerSound sounds
        |> Cmd.batch


playSound : String -> Cmd msg
playSound sound =
    makeSoundCmd "play" sound
        |> sendSoundCmd


sendSoundCmd : SoundCmd -> Cmd msg
sendSoundCmd soundCmd =
    soundCmd
        |> encodeSoundCmd
        |> audio


port audio : Value -> Cmd msg
