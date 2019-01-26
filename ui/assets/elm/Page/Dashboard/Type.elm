module Page.Dashboard.Type exposing (..)

import Data.Dashboard as Dashboard exposing (Content, Dashboard, DashboardShort,getDashboard, getTabs, decodeUpdateNoti)
import Data.Port as Port exposing (Port)
import Data.Light as Light exposing (Light)
import Data.Dimmer as Dimmer exposing (Dimmer)
import Data.Action as Action exposing (Action)
import Data.Sunblind as Sunblind exposing (Sunblind)
import Data.Task as Task exposing (Task)

import Material
import Material.Snackbar as Snackbar
import Array exposing (Array)
import Http
import Json.Decode exposing (Value)

type alias Model =
    { data : Dashboard
    , inProgress : Maybe Dashboard.Content
    , socketRef : Maybe Int
    , loaded : Bool
    , tabs : Array DashboardShort
    , selectedTab : Int
    , raise : Int
    , mdl : Material.Model
    , snackbar : Snackbar.Model Int
    , usedIndex : Int -- TODO change to dictionary
    }


type Msg
  = Mdl (Material.Msg Msg)
  | Snackbar (Snackbar.Msg Int)
  | SelectTab Int
  | LoadTabs (Result Http.Error (List DashboardShort))
  | LoadDashboard (Result Http.Error Dashboard)
  | Raise Int
  | PortToggle Port
  | LightToggle Light
  | SunblindToggle Sunblind
  | TaskToggle Task
  | SetDimmerFill Dimmer Float
  | DimLightToggle Dimmer Dimmer.Light
  | DimmerToggle Dimmer
  | ActionToggle Action
  | PortEdit Port
  | DimmerEdit Dimmer
  | SunblindEdit Sunblind
  | ActionEdit Action
  | LightEdit Light
  | TaskEdit Task
  | Skip
  | ResponseDimmer (Result Http.Error Dimmer)
  | ResponseLight (Result Http.Error Light)
  | ResponseSunblind (Result Http.Error Sunblind)
  | ResponsePort (Result Http.Error Port)
  | ResponseAction (Result Http.Error Action)
  | ResponseTask (Result Http.Error Task)
  | SunblindManualToggle Sunblind
  | ReceiveDashboardMessage Value
  | ShowToast String