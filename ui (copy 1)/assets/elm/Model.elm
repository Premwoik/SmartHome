module Model exposing (..)
import Material
import Http
import Data.Dimmer as DimmerM exposing (DimmerJson, Dimmer)
import Data.Sunblind as SunblindM exposing (Sunblind)
import Data.Light as LightM exposing (Light)

type alias Model =
    { dimmers : List Dimmer
    , sunblinds: List Sunblind
    , blindUndrawn : Bool
    , raised : Int
    , mdl : Material.Model
    }


type Msg =
    Raise Int
    | InitSunblinds (Result Http.Error (List Sunblind))
    | InitDimmers (Result Http.Error (List DimmerJson))
    | DimSlide Dimmer Float
    | DimToggle Dimmer
    | ToggleLight Dimmer Light
    | UndrawDimmer Dimmer
    | UndrawSunblinds
    | SetAllSunblinds
    | ToggleSunblind Sunblind
    | Mdl (Material.Msg Msg)