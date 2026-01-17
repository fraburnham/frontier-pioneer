module Data.Die exposing (..)

import Types exposing (..)


dieToString : Die -> String
dieToString die =
    case die of
        D4 ->
            "d4"

        D6 ->
            "d6"

        D8 ->
            "d8"

        D10 ->
            "d10"

        D12 ->
            "d12"

        D20 ->
            "d20"
