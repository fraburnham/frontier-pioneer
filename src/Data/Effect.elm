module Data.Effect exposing (..)

import Types exposing (..)


activeEffects : Model -> List Effect
activeEffects model =
    case model.temporaryEffect of
        Nothing ->
            model.effects

        Just e ->
            e :: model.effects
