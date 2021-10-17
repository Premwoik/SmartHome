IO.puts("Initializing MainRepo")

alias DB.Data.Action
alias DB.Data.Device
alias DB.Data.Port
alias DB.Data.ScheduleJob
alias DB.Data.RfButton
alias DB.Data.Page
alias DB.MainRepo, as: Repo

if [] == Device.list_all!() do
  %Device{id: 1, name: "Arduino-Mega", ip: "192.168.2.137", port: 80, type: :Arduino}
  |> Repo.insert()

  %Device{id: 2, name: "Integra", ip: "192.168.2.136", port: 9000, type: :Satel}
  |> Repo.insert()

  %Device{id: 3, name: "Shelly1-59D6AF", ip: "192.168.2.207", port: 80, type: :Shelly}
  |> Repo.insert()

  %Device{id: 4, name: "Shelly1-246E88", ip: "192.168.2.208", port: 80, type: :Shelly}
  |> Repo.insert()

  %Device{id: 5, name: "Shelly-Ledy-RGB", ip: "192.168.2.210", port: 80, type: :ShellyRGBW2}
  |> Repo.insert()

  %Device{id: 6, name: "SonoffRfBridge", ip: "192.168.2.122", port: 80, type: :SonoffRfBridge}
  |> Repo.insert()

  %Device{id: 7, name: "Kinkiet1-5EB49C", ip: "192.168.2.205", port: 80, type: :SonoffBasic}
  |> Repo.insert()

  %Device{id: 8, name: "Kinkiet2-EA3266", ip: "192.168.2.206", port: 80, type: :SonoffBasic}
  |> Repo.insert()

  %Device{id: 9, name: "Zasilanie-led", ip: "192.168.2.209", port: 80, type: :SonoffBasic}
  |> Repo.insert()

  IO.puts("Initializing devices!")
end

if [] == Port.list_all() do
  %Port{
    id: 1,
    name: "Jadalnia",
    number: 18,
    mode: :output,
    type: :dimmer,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 2,
    name: "Schody",
    number: 19,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 3,
    name: "Stół",
    number: 20,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 4,
    name: "Kanapa",
    number: 21,
    mode: :output,
    type: :dimmer,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 5,
    name: "Kanapa L",
    number: 22,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 6,
    name: "Kanapa P",
    number: 23,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 7,
    name: "TV",
    number: 24,
    mode: :output,
    type: :dimmer,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 8,
    name: "TV L",
    number: 25,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 9,
    name: "TV P",
    number: 26,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 10,
    name: "Mock",
    number: 34,
    mode: :output,
    type: :custom,
    state: %{"value" => false},
    device_id: 1
  }
  |> Repo.insert()

  sunblid_default_state = 
    %{"value" => false, 
      "position" => "open", 
      "move_duration" => 30_000}

  %Port{
    id: 11,
    name: "Parter",
    number: 35,
    mode: :output,
    type: :sunblind,
    state: sunblid_default_state,
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 12,
    name: "Taras Przemek",
    number: 36,
    mode: :output,
    type: :sunblind,
    state: sunblid_default_state,
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 13,
    name: "Balkon Przemek",
    number: 37,
    mode: :output,
    type: :sunblind,
    state: sunblid_default_state,
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 14,
    name: "Balkon Michał",
    number: 38,
    mode: :output,
    type: :sunblind,
    state: sunblid_default_state,
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 15,
    name: "Salon",
    number: 39,
    mode: :output,
    type: :sunblind,
    state: sunblid_default_state,
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 16,
    name: "Taras Salon",
    number: 40,
    mode: :output,
    type: :sunblind,
    state: sunblid_default_state,
    device_id: 1
  }
  |> Repo.insert()

  %Port{
    id: 17,
    name: "Rf nieprzypisany",
    number: 1,
    mode: :output,
    type: :custom,
    state: %{"value" => false},
    device_id: 6
  }
  |> Repo.insert()

  %Port{
    id: 18,
    name: "Salon w rogu",
    number: 3,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 6
  }
  |> Repo.insert()

  %Port{
    id: 19,
    name: "Salon komoda",
    number: 5,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 6
  }
  |> Repo.insert()

  %Port{
    id: 20,
    name: "Strych lampa stojąca",
    number: 7,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 6
  }
  |> Repo.insert()

  %Port{
    id: 21,
    name: "Rf nieprzypisany",
    number: 9,
    mode: :output,
    type: :custom,
    state: %{"value" => false},
    device_id: 6
  }
  |> Repo.insert()

  %Port{
    id: 22,
    name: "Rf nieprzypisany",
    number: 11,
    mode: :output,
    type: :custom,
    state: %{"value" => false},
    device_id: 6
  }
  |> Repo.insert()

  %Port{
    id: 23,
    name: "Sypialnia",
    number: 13,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 6
  }
  |> Repo.insert()

  %Port{
    id: 24,
    name: "Wiatrak łazienka",
    number: 15,
    mode: :output,
    type: :custom,
    state: %{"value" => false},
    device_id: 6
  }
  |> Repo.insert()

  %Port{
    id: 25,
    name: "Poddasze główne",
    number: 0,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 4
  }
  |> Repo.insert()

  %Port{
    id: 26,
    name: "Poddasze gablota",
    number: 0,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 3
  }
  |> Repo.insert()

  %Port{
    id: 27,
    name: "Kinkiet od okna",
    number: 0,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 8
  }
  |> Repo.insert()

  %Port{
    id: 28,
    name: "Kinkiet od szafy",
    number: 0,
    mode: :output,
    type: :light,
    state: %{"value" => false},
    device_id: 7
  }
  |> Repo.insert()

  %Port{
    id: 29,
    name: "Poddasze ledy belka",
    number: 0,
    mode: :output,
    type: :dimmer,
    state: %{"value" => false},
    device_id: 5
  }
  |> Repo.insert()

  %Port{
    id: 30,
    name: "Zasilanie led",
    number: 0,
    mode: :output,
    type: :custom,
    state: %{"value" => false},
    device_id: 9
  }
  |> Repo.insert()

  %Port{
    id: 31,
    name: "Sygnal ciepła woda",
    number: 60,
    mode: :input,
    type: :signal,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 32,
    name: "Sygnal zamykanie rolet",
    number: 62,
    mode: :input,
    type: :signal,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 33,
    name: "Piwnica przedpokój",
    number: 1,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 34,
    name: "Piwnica",
    number: 2,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 35,
    name: "Piwnica kuchnia",
    number: 3,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 36,
    name: "Piwnica kotłownia",
    number: 4,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 37,
    name: "Parter łazienka",
    number: 5,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 38,
    name: "Parter sypialnia",
    number: 6,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 39,
    name: "Parter",
    number: 7,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 40,
    name: "Salon schody",
    number: 8,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 41,
    name: "Salon stół",
    number: 9,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 42,
    name: "Salon",
    number: 10,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 43,
    name: "Salon kuchnia",
    number: 11,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 44,
    name: "Salon przedpokój",
    number: 12,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 45,
    name: "Piętro Przemek",
    number: 13,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 46,
    name: "Piętro Michał",
    number: 14,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 47,
    name: "Piętro łazienka",
    number: 15,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  %Port{
    id: 48,
    name: "Strych",
    number: 16,
    mode: :input,
    type: :motion_sensor,
    state: %{"value" => false},
    device_id: 2
  }
  |> Repo.insert()

  IO.puts("Initializing ports!")
end

if [] == Action.list_all() do
  %Action{
    id: 1,
    name: "Cały dom",
    state: true,
    module: "CloseSunblind",
    pause: 15000,
    attributes: %{"up" => [11, 12, 13, 14, 15, 16], "down" => [11, 14, 15, 16]}
  }
  |> Repo.insert()

  %Action{
    id: 2,
    name: "Salon schody",
    state: false,
    module: "AutoLights",
    pause: 15000,
    activator_id: 18,
    attributes: %{"duration" => 30000}
  }
  |> Repo.insert()

  %Action{
    id: 3,
    name: "Salon stół",
    state: false,
    module: "AutoLights",
    pause: 15000,
    activator_id: 41,
    attributes: %{"duration" => 30000}
  }
  |> Repo.insert()

  %Action{
    id: 4,
    name: "Salon wypoczynek",
    state: false,
    module: "AutoLights",
    pause: 15000,
    activator_id: 42,
    attributes: %{"duration" => 30000}
  }
  |> Repo.insert()

  %Action{
    id: 5,
    name: "Pokój Przemek",
    state: true,
    module: "CloseSunblind",
    pause: 15000,
    attributes: %{"down" => [12, 13]}
  }
  |> Repo.insert()

  %Action{
    id: 6,
    name: "Sypialnia światło",
    state: false,
    module: "AutoLights",
    activator_id: 38,
    pause: 15000,
    attributes: %{"duration" => 30000}
  }
  |> Repo.insert()

  %Action{
    id: 7,
    name: "Światło poddasze",
    state: true,
    module: "AutoLights",
    pause: 15000,
    activator_id: 48,
    attributes: %{"duration" => 30_000}
  }
  |> Repo.insert()

  %Action{
    id: 8,
    name: "Wiatrak łazienka",
    state: false,
    module: "AutoLights",
    pause: 1000,
    activator_id: 47,
    attributes: %{"duration" => 10000}
  }
  |> Repo.insert()

  %Action{
    id: 9,
    name: "Grupa świateł salon",
    state: true,
    module: "ToggleGroup",
    pause: 1000,
    attributes: %{
      "ports" => [5, 6, 8, 9]
    }
  }
  |> Repo.insert()

  %Action{
    id: 10,
    name: "Sterowanie ściemniaczem TV",
    state: true,
    module: "DimmerController"
  }
  |> Repo.insert()

  %Action{
    id: 11,
    name: "Sterowanie ściemniaczem Kanapa",
    state: true,
    module: "DimmerController"
  }
  |> Repo.insert()

  %Action{
    id: 12,
    name: "Odczyt wejść satel",
    state: true,
    module: "ReadInputs",
    attributes: %{"device_id" => 2}
  }
  |> Repo.insert()

  %Action{
    id: 13,
    name: "Odczyt temperatury mega",
    state: true,
    module: "ReadTemperature",
    attributes: %{"device_id" => 1}
  }
  |> Repo.insert()

  %Action{
    id: 14,
    name: "Heartbeat mega",
    state: true,
    module: "Heartbeat",
    attributes: %{"device_id" => 1}
  }
  |> Repo.insert()

  IO.puts("Initializing actions!")
end

if [] == ScheduleJob.list_all!() do
  %ScheduleJob{
    id: 1,
    name: "",
    expr: "*/2 * * * * *",
    extended: true,
    task: %{"action_id" => 12}
  }
  |> Repo.insert()

  %ScheduleJob{
    id: 2,
    name: "",
    expr: "* 17 * * * *",
    task: %{"action_id" => 1, "state" => "up"}
  }
  |> Repo.insert()

  %ScheduleJob{
    id: 3,
    name: "",
    expr: "* 8 * * * *",
    task: %{"action_id" => 1, "state" => "down"}
  }
  |> Repo.insert()

  %ScheduleJob{
    id: 4,
    name: "",
    expr: "* 10 * * * *",
    task: %{"action_id" => 5, "state" => "down"}
  }
  |> Repo.insert()

  %ScheduleJob{
    id: 5,
    name: "",
    expr: "*/1 * * * * *",
    task: %{"action_id" => 14}
  }
  |> Repo.insert()

  %ScheduleJob{
    id: 6,
    name: "",
    expr: "*/30 * * * * *",
    task: %{"action_id" => 13}
  }
  |> Repo.insert()

  IO.puts("Initializing jobs!")
end

if [] == RfButton.list_all!() do
  %RfButton{
    name: "8365000A2C-1",
    key_value: "1198E8",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 2},
        2 => %{"type" => "port", "id" => 2}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365000A2C-2",
    key_value: "1198EC",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 3},
        2 => %{"type" => "port", "id" => 19}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365000A2C-3",
    key_value: "1198E4",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 8},
        2 => %{"type" => "action", "id" => 10}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365000A2C-4",
    key_value: "1198E9",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 9},
        2 => %{"type" => "action", "id" => 11}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365000A2C-5",
    key_value: "1198E2",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 5}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365000A2C-6",
    key_value: "1198E5",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 6}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365000A2C-7",
    key_value: "1198E1",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 9}
      }
    }
  }
  |> Repo.insert()

  %RfButton{name: "8365000A2C-8", key_value: "1198E3", mode: :page}
  |> Repo.insert()

  %RfButton{
    name: "836500138C-1",
    key_value: "6195B8",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 23}
      }
    }
  }
  |> Repo.insert()

  %RfButton{name: "836500138C-2", key_value: "6195BC"}
  |> Repo.insert()
  %RfButton{name: "836500138C-3", key_value: "6195B4"}
  |> Repo.insert()
  %RfButton{name: "836500138C-4", key_value: "6195B9"}
  |> Repo.insert()
  %RfButton{name: "836500138C-5", key_value: "6195B2"}
  |> Repo.insert()
  %RfButton{name: "836500138C-6", key_value: "6195B5"}
  |> Repo.insert()
  %RfButton{name: "836500138C-7", key_value: "6195B1"}
  |> Repo.insert()
  %RfButton{name: "836500138C-8", key_value: "6195B3"}
  |> Repo.insert()
  %RfButton{
    name: "8365001354-1",
    key_value: "9B6758",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 25}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365001354-2",
    key_value: "9B675C",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 26}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365001354-3",
    key_value: "9B6754",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 27}
      }
    }
  }
  |> Repo.insert()

  %RfButton{
    name: "8365001354-4",
    key_value: "9B6759",
    on_click_action: %{
      "pages" => %{
        1 => %{"type" => "port", "id" => 28}
      }
    }
  }
  |> Repo.insert()

  %RfButton{name: "8365001354-5", key_value: "9B6752"}
  |> Repo.insert()
  %RfButton{name: "8365001354-6", key_value: "9B6755"}
  |> Repo.insert()
  %RfButton{name: "8365001354-7", key_value: "9B6751"}
  |> Repo.insert()
  %RfButton{name: "8365001354-8", key_value: "9B6753"}
  |> Repo.insert()

  IO.puts("Initializing rf_buttons!")
end

if [] == Page.list_all!() do
  %Page{
    id: 1,
    name: "Piwnica",
    order: 1
  }
  |> Repo.insert()

  Repo.insert_all("page_ports", [
    %{page_id: 1, port_id: 33},
    %{page_id: 1, port_id: 34},
    %{page_id: 1, port_id: 35},
    %{page_id: 1, port_id: 36}
  ])

  %Page{
    id: 2,
    name: "Parter",
    order: 2
  }
  |> Repo.insert()

  Repo.insert_all("page_ports", [
    %{page_id: 2, port_id: 10},
    %{page_id: 2, port_id: 23},
    %{page_id: 2, port_id: 37},
    %{page_id: 2, port_id: 38},
    %{page_id: 2, port_id: 39}
  ])

  Repo.insert_all("page_actions", [%{page_id: 2, action_id: 6}])

  %Page{
    id: 3,
    name: "Salon",
    order: 3
  }
  |> Repo.insert()

  Repo.insert_all("page_ports", [
    %{page_id: 3, port_id: 1},
    %{page_id: 3, port_id: 2},
    %{page_id: 3, port_id: 3},
    %{page_id: 3, port_id: 4},
    %{page_id: 3, port_id: 5},
    %{page_id: 3, port_id: 6},
    %{page_id: 3, port_id: 7},
    %{page_id: 3, port_id: 8},
    %{page_id: 3, port_id: 9},
    %{page_id: 3, port_id: 18},
    %{page_id: 3, port_id: 19},
    %{page_id: 3, port_id: 15},
    %{page_id: 3, port_id: 16},
    %{page_id: 3, port_id: 40},
    %{page_id: 3, port_id: 41},
    %{page_id: 3, port_id: 42},
    %{page_id: 3, port_id: 43},
    %{page_id: 3, port_id: 44}
  ])

  Repo.insert_all("page_actions", [
    %{page_id: 3, action_id: 2},
    %{page_id: 3, action_id: 3},
    %{page_id: 3, action_id: 4},
    %{page_id: 3, action_id: 9},
    %{page_id: 3, action_id: 10},
    %{page_id: 3, action_id: 11}
  ])

  %Page{
    id: 4,
    name: "Piętro",
    order: 4
  }
  |> Repo.insert()

  Repo.insert_all("page_ports", [
    %{page_id: 4, port_id: 12},
    %{page_id: 4, port_id: 13},
    %{page_id: 4, port_id: 14},
    %{page_id: 4, port_id: 24},
    %{page_id: 4, port_id: 45},
    %{page_id: 4, port_id: 46},
    %{page_id: 4, port_id: 47}
  ])

  Repo.insert_all("page_actions", [%{page_id: 4, action_id: 5}, %{page_id: 4, action_id: 8}])

  %Page{
    id: 5,
    name: "Poddasze",
    order: 5
  }
  |> Repo.insert()

  Repo.insert_all("page_ports", [
    %{page_id: 5, port_id: 20},
    %{page_id: 5, port_id: 25},
    %{page_id: 5, port_id: 26},
    %{page_id: 5, port_id: 27},
    %{page_id: 5, port_id: 28},
    %{page_id: 5, port_id: 29},
    %{page_id: 5, port_id: 30},
    %{page_id: 5, port_id: 48}
  ])

  Repo.insert_all("page_actions", [%{page_id: 5, action_id: 7}])

  IO.puts("Initializing page!")
end
