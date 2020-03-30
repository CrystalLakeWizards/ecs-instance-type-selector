import Dict exposing (Dict)
import Dict.Extra exposing (find)

type alias Cluster =
    {
        name : String
    }

type alias Service = 
    {
        clusterId : Int
        , name : String
    }


Dict.fromList [ ( 0, Service 0 "Test Service" ) ] |> find (\_ service -> service.clusterId == 0)