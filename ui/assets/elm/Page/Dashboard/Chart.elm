module Page.Dashboard.Chart exposing (ch1, ch2, chart)

import LineChart
import LineChart.Colors as Colors
import LineChart.Junk as Junk
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Junk as Junk
import LineChart.Dots as Dots
import LineChart.Grid as Grid
import LineChart.Dots as Dots
import LineChart.Line as Line
import LineChart.Colors as Colors
import LineChart.Events as Events
import LineChart.Legends as Legends
import LineChart.Container as Container
import LineChart.Interpolation as Interpolation
import LineChart.Axis.Intersection as Intersection
import Page.Dashboard.Type exposing (..)

import Html exposing (..)
import Material.Toggles as Toggles
import Material.Options as Options

type alias Point =
  { x : Float, y : Float }



points =
       [ Point 0 20
        , Point 1 20.5
        , Point 2 21
        , Point 3 21
        , Point 4 21
        , Point 5 21
        , Point 6 21
        , Point 7 21
        , Point 8 21
        , Point 9 21.5
        , Point 10 21.5
        , Point 11 22
        , Point 12 22
        , Point 13 22
        , Point 14 21.5
        , Point 15 21.5
        , Point 16 21
        , Point 17 21
        , Point 18 20.5
        , Point 19 20
        , Point 20 20
        , Point 21 21
        , Point 22 21
        , Point 23 21
        ]
points2 =
        [ Point 1 2.177
        , Point 2 3.001
        , Point 3 1.232
        , Point 4 2.231
        , Point 5 2.123
        , Point 6 2.532
        , Point 7 2.342
        ]


chart : Model -> String -> Html Msg -> Bool -> Bool -> Bool -> Html Msg
chart model n chart t1 t2 t3=
  Html.div []
  [ h5 [] [text n]
  , Toggles.radio Mdl [6,0] model.mdl
        [ Toggles.value t1
        , Toggles.group "MyRadioGroup"
        , Toggles.ripple
        , Options.css "margin-right" "10px"
--        , Options.onToggle MyRadioMsg1
        ]
        [ text "dzień" ]
    , Toggles.radio Mdl [6,1] model.mdl
        [ Toggles.value t2
        , Toggles.group "MyRadioGroup"
        , Toggles.ripple
        , Options.css "margin-right" "10px"
--        , Options.onToggle MyRadioMsg2
        ]
        [ text "tydzień" ]
    , Toggles.radio Mdl [6,1] model.mdl
         [ Toggles.value t3
         , Toggles.group "MyRadioGroup"
         , Toggles.ripple
 --        , Options.onToggle MyRadioMsg2
         ]
         [ text "miesiąc" ]
  , chart
    ]

ch1 : Html Msg
ch1 =
      LineChart.viewCustom
          { y = Axis.default 350 "°C" .y
          , x = Axis.picky 650 "Godzina" .x [0,2,4,6,8,10,12,14,16,18,20,22]
          , container = Container.default "line-chart-1"
          , interpolation = Interpolation.default
          , intersection = Intersection.default
          , legends = Legends.default
          , events = Events.default
          , junk = Junk.default
          , grid = Grid.default
          , area = Area.default -- Changed from the default!
          , line = Line.default
          , dots = Dots.default
          }
        [ LineChart.line Colors.pinkLight Dots.circle "Termometr 1" points
        ]
ch2 : Html Msg
ch2 =
    LineChart.viewCustom
        { y = Axis.default 350 "kWh" .y
        , x = Axis.picky 650 "Dzień" .x [1,2,3,4,5,6,7]
        , container = Container.default "line-chart-1"
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.default
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default -- Changed from the default!
        , line = Line.default
        , dots = Dots.default
        }
      [ LineChart.line Colors.pinkLight Dots.circle "Watomierz 1" points2
      ]

