module Turf exposing (..)

import GeoJson exposing (Position)
import Tuple3


getLon =
    Tuple3.first


getLat =
    Tuple3.second


getDistance : Position -> Position -> Float
getDistance coord1 coord2 =
    let
        dLat =
            radians <| getLat coord2 - getLat coord1

        dLon =
            radians <| getLon coord2 - getLon coord1

        lat1 =
            radians <| getLat coord1

        lat2 =
            radians <| getLat coord2

        a =
            sin (dLat / 2) ^ 2 |> (+) ((sin (dLon / 2) ^ 2) * cos lat1 * cos lat2)

        c =
            2 * atan2 (sqrt a) (sqrt <| 1 - a)
    in
    c * 6373
