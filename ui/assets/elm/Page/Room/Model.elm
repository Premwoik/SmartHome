module Page.Room.Model exposing (..)

import Data.Dimmer exposing (Dimmer, DimmerJson)
import Data.Sunblind exposing (Sunblind)
import Data.Light exposing (Light)
import Material
import Http


type alias Model =
    { dimmers : List Dimmer
    , sunblinds: List Sunblind
    , blindUndrawn : Bool
    , raised : Int
    , mdl : Material.Model
    }

model : Model
model =
    { dimmers = []
    , sunblinds = []
    , blindUndrawn = False
    , raised = -1
    , mdl = Material.model
    }

type Msg =
    Raise Int
    | InitRoom (Result Http.Error (List Sunblind, List DimmerJson))
    | Response (Result Http.Error String)
    | DimSlide Dimmer Float
    | DimToggle Dimmer
    | ToggleLight Dimmer Light
    | UndrawDimmer Dimmer
    | UndrawSunblinds
    | SetAllSunblinds
    | ToggleSunblind Sunblind
    | Mdl (Material.Msg Msg)