module Admin.Data exposing (..)


import Admin.Action exposing (Action)
import Admin.Device exposing (Device)
import Admin.Dimmer exposing (Dimmer)
import Admin.Port exposing (Port)
import Material


type alias Item = ( Int, String, String, String )

type TabType
    = TabCreator
    | TabActions
    | TabDevices
    | TabPorts
    | TabDimmers

type EditData
    = EditActionData Action
    | EditDeviceData Device
    | EditDimmerData Dimmer
    | EditPortData Port
    | None

type Edit
    = EditAction EditActionField
    | EditDimmer EditDimmerField
    | EditDevice EditDeviceField
    | EditPort EditPortField

type EditActionField
    = EditActionName String
    | EditActionFunction String
    | EditActionActive Bool
    | EditActionParams String
    | EditActionFrequency (Result String Int)
    | EditActionStartTime String
    | EditActionEndTime String
--    | EditActionPort Int

type EditPortField
    = EditPortName String
    | EditPortMode String
    | EditPortState Bool
    | EditPortType String
    | EditPortTimeout (Result String Int)
--    | EditPortDeviceId Int

type EditDeviceField
    = EditDeviceName String
    | EditDeviceIp String
    | EditDevicePort (Result String Int)
    | EditDeviceType String

type EditDimmerField
    = EditDimmerName String
    | EditDimmerType String
    | EditDimmerTime (Result String Int)
    | EditDimmerFullTime (Result String Int)
    | EditDimmerFill (Result String Int)

type CreatorAdvance
    = ActionCreator
    | DimmerCreator
    | PortCreator
    | DeviceCreator
    | LightCreator
    | NoneCreator




type alias Model =
    { selectedTab : Int
    , mdl : Material.Model
    , tab : TabType
    , creator : Maybe Int
    , devices : List Device
    , ports : List Port
    , dimmers : List Dimmer
    , actions : List Action
    , editIndex : Int
    , editData : EditData
    , creatorAdv : CreatorAdvance
    }



