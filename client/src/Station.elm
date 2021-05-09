module Station exposing (..)

import Json.Decode as D
import Maybe.Extra exposing (isJust)
import String exposing (contains, indexes, toUpper)
import Tuple exposing (pair)
import Tuple3
import GeoJson exposing (Position)


type alias Station =
    { code : String
    , name : String
    , coStations : CoStations
    , lineCodes : LineCodes
    , latLng : Position
    }


decodeStation : D.Decoder Station
decodeStation =
    D.map5
        Station
        (D.at [ "code" ] D.string)
        (D.at [ "name" ] D.string)
        (D.map2
            CoStations
            (D.at [ "coStation1" ] <| D.maybe D.string)
            (D.at [ "coStation2" ] <| D.maybe D.string)
        )
        (D.map3
            LineCodes
            (D.at [ "lineCode1" ] D.string)
            (D.at [ "lineCode2" ] <| D.maybe D.string)
            (D.at [ "lineCode3" ] <| D.maybe D.string)
        )
        (D.map3 Tuple3.join
            (D.at [ "latitude" ] D.float)
            (D.at [ "longitude" ] D.float)
            (D.succeed 0)
        )


type alias CoStations =
    { coStation1 : Maybe String
    , coStation2 : Maybe String
    }


type alias LineCodes =
    { lineCode1 : String
    , lineCode2 : Maybe String
    , lineCode3 : Maybe String
    }


searchStation : String -> List Station -> List Station
searchStation query =
    List.filter
        (.name >> toUpper >> contains (toUpper query))
        >> List.filter (always <| query /= "")
        >> List.sortBy .name
        >> List.sortBy
            (.name
                >> toUpper
                >> indexes (toUpper query)
                >> List.head
                >> Maybe.withDefault -1
            )


findCoStation : Station -> List Station -> Maybe Station
findCoStation station stations =
    station.coStations.coStation1
        |> Maybe.andThen
            (\code ->
                List.filter (.code >> (==) code) stations |> List.head
            )


lineCodeDisplay : Station -> Maybe String
lineCodeDisplay station =
    case station.coStations.coStation1 of
        Just _ ->
            [ Just station.lineCodes.lineCode1
            , station.lineCodes.lineCode2
            , station.lineCodes.lineCode3
            ]
                |> List.filter isJust
                |> List.map (Maybe.withDefault "")
                |> String.join ", "
                |> Just

        Nothing ->
            Nothing
