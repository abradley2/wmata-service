module Turf exposing (..)


type alias Coord =
    ( Float, Float )


{-
  var dLat = toRad(coordinates2[LAT] - coordinates1[LAT]);
  var dLon = toRad(coordinates2[LON] - coordinates1[LON]);
  var lat1 = toRad(coordinates1[LAT]);
  var lat2 = toRad(coordinates2[LAT]);

  var a = Math.pow(Math.sin(dLat/2), 2) +
          Math.pow(Math.sin(dLon/2), 2) * Math.cos(lat1) * Math.cos(lat2);
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
-}

getLat = Tuple.first

getLon = Tuple.second

getDistance : Coord -> Coord -> Float
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
            2 * (atan2 (sqrt a) (sqrt <| 1 - a))
    in
    c * 6373 -- let's just use KM here
