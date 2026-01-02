module Data.Damage exposing (..)

import Types exposing (..)


shipImproved : Model -> Int -> Int
shipImproved model =
    case List.member ShipImproved model.effects of
        False ->
            identity

        True ->
            \d -> d - 1


damageAmount : Model -> TurnState -> Int
damageAmount model turnState =
    -- This isn't gonna compose w/ the empty sector damage. Write the fn then think about it
    shipImproved model turnState.roll.d6
