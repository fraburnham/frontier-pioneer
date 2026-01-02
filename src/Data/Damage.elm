module Data.Damage exposing (..)

import Types exposing (..)


damageAmount : Model -> TurnState -> Int
damageAmount model turnState =
    case List.member ShipImproved model.effects of
        False ->
            turnState.roll.d6

        True ->
            turnState.roll.d6 - 1
