defmodule DB.Init do
  @moduledoc nil

  alias DB.{
    Light,
    Task,
    TaskType,
    Watcher,
    Dimmer,
    Port,
    Device,
    DeviceType,
    Action,
    Repo,
    Sunblind,
    Page,
    PageContent,
    Thermometer
    }

  import Ecto.Query
  require Logger

  def start_link, do: {:ok, spawn_link(__MODULE__, :run, [])}

  def run() do
    Logger.info("Creating data!")
    :ok = delete_all()

    if !exist? do
      :ok = insert_ard_mega()
      :ok = insert_integra64()
      :ok = insert_shelly()
      :ok = insert_sonoff_rf()
      :ok = insert_tasks()
      :ok = init_pages()
      :ok = init_therm()
      :ok = init_buttons()
      :ok
    end

    Logger.info("Data created!")
  end

  defp exist? do
    length(Repo.all(Port)) > 0
  end

  def delete_all() do
    #    Repo.delete_all(Light)
    #    Repo.delete_all(Dimmer)
    #    Repo.delete_all(Watcher)
    #    Repo.delete_all(Action)
    #    Repo.delete_all(Port)
    #    Repo.delete_all(Device)
    #    Repo.delete_all(TaskType)
    #    Repo.delete_all(Task)
    :ok
  end

  def insert_sonoff_basic() do
    type =
      %DeviceType{
        name: "Sonoff Basic",
        module: "Core.Device.SonoffBasic",
        process: false
      }
      |> Repo.insert!()

    p33 = %Port{
      name: "Poddasze kinkiet 1",
      type: "light",
      number: 0,
      mode: "output",
      inverted_logic: false,
      state: false
    }
    %Device{
      name: "Kinkiet1-5EB49C",
      ip: "192.168.2.205",
      port: 80,
      alive: true,
      type_id: type.id,
      ports: [p33]
    }
    |> Repo.insert!()

    p34 = %Port{
      name: "Poddasze kinkiet 2",
      type: "light",
      number: 0,
      mode: "output",
      inverted_logic: false,
      state: false
    }
      %Device{
        name: "Kinkiet2-EA3266",
        ip: "192.168.2.206",
        port: 80,
        alive: true,
        type_id: type.id,
        ports: [p34]
      }
      |> Repo.insert!()

      :ok
  end

  def insert_sonoff_rf() do
    type =
      %DeviceType{
        name: "Sonoff Rf Bridge",
        module: "Core.Device.SonoffRfBridge",
        process: false
      }
      |> Repo.insert!()


    p25 = %Port{
      name: "Rf wolna",
      type: "light",
      number: 1,
      mode: "output",
      inverted_logic: false,
      state: false
    }

    p26 = %Port{
      name: "Salon w rogu",
      type: "light",
      number: 3,
      mode: "output",
      inverted_logic: false,
      state: false
    }

    p27 = %Port{
      name: "Salon komoda",
      type: "light",
      number: 5,
      mode: "output",
      inverted_logic: false,
      state: false
    }

    p28 = %Port{
      name: "Strych lampa stojąca",
      type: "light",
      number: 7,
      mode: "output",
      inverted_logic: false,
      state: false
    }
    p29 = %Port{
      name: "Rf wolna",
      type: "light",
      number: 9,
      mode: "output",
      inverted_logic: false,
      state: false
    }
    p30 = %Port{
      name: "Rf wolna",
      type: "light",
      number: 11,
      mode: "output",
      inverted_logic: false,
      state: false
    }
    p31 = %Port{
      name: "Sypialnia",
      type: "light",
      number: 13,
      mode: "output",
      inverted_logic: false,
      state: false
    }
    p32 = %Port{
      name: "Rf wolna",
      type: "light",
      number: 15,
      mode: "output",
      inverted_logic: false,
      state: false
    }
    dev =
      %Device{
        name: "Sonoff Rf Bridge",
        ip: "192.168.2.122",
        port: 80,
        alive: true,
        type_id: type.id,
        ports: [p25, p26, p27, p28, p29, p30, p31, p32]
      }
      |> Repo.insert!()

    %Light{port_id: 28, dimmer_id: nil}
    |> Repo.insert!()

    %Light{port_id: 26, dimmer_id: nil}
    |> Repo.insert!()

    %Light{port_id: 27, dimmer_id: nil}
    |> Repo.insert!()

    %Light{port_id: 31, dimmer_id: nil}
    |> Repo.insert!()

    :ok
  end

  def insert_shelly() do
    type =
      %DeviceType{
        name: "Shelly",
        module: "Core.Device.Shelly",
        process: false
      }
      |> Repo.insert!()
    type2 =
      %DeviceType{
        name: "ShellyRGBW2",
        module: "Core.Device.ShellyRGBW2",
        process: false
      }
      |> Repo.insert!()

    p1 = %Port{
      name: "Test S1",
      type: "light",
      number: 0,
      mode: "output",
      inverted_logic: false,
      state: false
    }

    p2 = %Port{
      name: "Test S2",
      type: "light",
      number: 0,
      mode: "output",
      inverted_logic: false,
      state: false
    }
    p3 = %Port{
      name: "Ledy belka",
      type: "dimmer_rgbw",
      number: 0,
      mode: "output",
      inverted_logic: false,
      state: false
    }

    dev1 =
      %Device{
        name: "shelly1",
        ip: "192.168.2.138",
        port: 80,
        alive: true,
        type_id: type.id,
        ports: [p1]
      }
      |> Repo.insert!()

    dev2 =
      %Device{
        name: "shelly2",
        ip: "192.168.2.120",
        port: 80,
        alive: true,
        type_id: type.id,
        ports: [p2]
      }
      |> Repo.insert!()
    dev3 =
      %Device{
        name: "shellyRGBW2",
        ip: "192.168.2.122",
        port: 80,
        alive: true,
        type_id: type2.id,
        ports: [p3]
      }
      |> Repo.insert!()

    %Dimmer{port_id: 24, fill: 0, lights: []}
    |> Repo.insert!()


    %Light{port_id: 22, dimmer_id: nil}
    |> Repo.insert!()

    %Light{port_id: 23, dimmer_id: nil}
    |> Repo.insert!()

    :ok
  end

  def insert_ard_mega() do
    # 1
    p2 = %Port{name: "mock", type: "undef", number: 2, mode: "undef", state: nil}
    # 2
    p18 = %Port{
      name: "Jadalnia",
      type: "dimmer",
      number: 18,
      mode: "output",
      timeout: 4_500,
      inverted_logic: true,
      state: false
    }

    # 3
    p19 = %Port{
      name: "Schody",
      type: "light",
      number: 19,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 4
    p20 = %Port{
      name: "Stół",
      type: "light",
      number: 20,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 5
    p21 = %Port{
      name: "Kanapa",
      type: "dimmer",
      number: 21,
      mode: "output",
      timeout: 4_500,
      inverted_logic: true,
      state: false
    }

    # 6
    p22 = %Port{
      name: "Lewa",
      type: "light",
      number: 22,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 7
    p23 = %Port{
      name: "Prawa",
      type: "light",
      number: 23,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 8
    p24 = %Port{
      name: "Tv",
      type: "dimmer",
      number: 24,
      mode: "output",
      timeout: 4_500,
      inverted_logic: true,
      state: false
    }

    # 9
    p25 = %Port{
      name: "Lewa",
      type: "light",
      number: 25,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 10
    p26 = %Port{
      name: "Prawa",
      type: "light",
      number: 26,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 11
    p34 = %Port{
      name: "Parter open",
      type: "sunblind_helper",
      number: 34,
      mode: "output",
      timeout: 1000,
      inverted_logic: true,
      state: false
    }

    # 12
    p35 = %Port{
      name: "Parter close",
      type: "sunblind",
      number: 35,
      mode: "output",
      timeout: 1000,
      inverted_logic: true,
      state: false
    }

    # 13
    p36 = %Port{
      name: "TarasP",
      type: "sunblind",
      number: 36,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 14
    p37 = %Port{
      name: "BalkonP",
      type: "sunblind",
      number: 37,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 15
    p38 = %Port{
      name: "Michał",
      type: "sunblind",
      number: 38,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 16
    p39 = %Port{
      name: "Salon",
      type: "sunblind",
      number: 39,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    # 17
    p40 = %Port{
      name: "TarasSalon",
      type: "sunblind",
      number: 40,
      mode: "output",
      inverted_logic: true,
      state: false
    }

    type =
      %DeviceType{
        name: "Default",
        module: "Core.Device.Default",
        process: true
      }
      |> Repo.insert!()

    dev1 =
      %Device{
        name: "ard_mega",
        ip: "192.168.2.137",
        port: 1000,
        alive: true,
        type_id: type.id,
        ports: [
          p2,
          p18,
          p19,
          p20,
          p21,
          p22,
          p23,
          p24,
          p25,
          p26,
          p34,
          p35,
          p36,
          p37,
          p38,
          p39,
          p40
        ]
      }
      |> Repo.insert!()

    %Sunblind{port_id: 12, open_port_id: 11, type: "pulse2", full_open_time: 16_000}
    |> Repo.insert!()

    # %Sunblind{port_id: 12, type: "pulse", full_open_time: 16_000}
    # |> Repo.insert!

    %Sunblind{port_id: 13, full_open_time: 30_000}
    |> Repo.insert!()

    %Sunblind{port_id: 14, full_open_time: 30_000}
    |> Repo.insert!()

    %Sunblind{port_id: 15, full_open_time: 30_000}
    |> Repo.insert!()

    %Sunblind{port_id: 16, full_open_time: 30_000}
    |> Repo.insert!()

    %Sunblind{port_id: 17, full_open_time: 30_000}
    |> Repo.insert!()

    # 1
    l1 = %Light{port_id: 3, dimmer_id: 2}
    # 2
    l2 = %Light{port_id: 4, dimmer_id: 2}
    # 3
    l3 = %Light{port_id: 6, dimmer_id: 5}
    # 4
    l4 = %Light{port_id: 7, dimmer_id: 5}
    # 5
    l5 = %Light{port_id: 9, dimmer_id: 8}
    # 6
    l6 = %Light{port_id: 10, dimmer_id: 8}

    %Dimmer{port_id: 2, fill: 0, lights: [l1, l2]}
    |> Repo.insert!()

    %Dimmer{port_id: 5, fill: 0, lights: [l3, l4]}
    |> Repo.insert!()

    %Dimmer{port_id: 8, fill: 0, lights: [l5, l6]}
    |> Repo.insert!()

    #%Light{
    #port_id: 23,
    #dimmer_id: nil
    #}
    #|> Repo.insert!()

    #%Light{
    #port_id: 22,
    #dimmer_id: nil
    #}
    #|> Repo.insert!()

    :ok
  end

  def insert_integra64(test \\ false) do
    # 19
    p8 = %Port{
      name: "CzujkaSalonSchody",
      type: "motion_sensor",
      number: 8,
      mode: "input",
      state: nil
    }

    # 20
    p9 = %Port{
      name: "CzujkaSalonKuchnia",
      type: "motion_sensor",
      number: 9,
      mode: "input",
      state: nil
    }

    # 21
    p10 = %Port{name: "CzujkaSalon", type: "motion_sensor", number: 10, mode: "input", state: nil}
    # 22
    p62 = %Port{
      name: "SygnalZamknieciaRolet",
      type: "alarm_input",
      number: 62,
      mode: "input",
      state: nil
    }

    type =
      %DeviceType{
        name: "Satel",
        module: "Core.Device.Satel",
        process: true
      }
      |> Repo.insert!()

    dev1 =
      %Device{
        name: "integra",
        ip: "192.168.2.136",
        port: 9000,
        type_id: type.id,
        alive: true,
        ports: [p8, p9, p10, p62]
      }
      |> Repo.insert!()

    sunblindsClose = Repo.all(from(p in Port, where: p.id in [12, 13, 14, 15, 16, 17]))
    sunblindsOpen = Repo.all(from(p in Port, where: p.id in [12, 15, 16, 17]))
    sunblindsPrzemek = Repo.all(from(p in Port, where: p.id in [13, 14]))

    type1 = "CloseSunblinds"
    type2 = "AutoLights"
    # CałyDom - RoletyZamykanie id: 1
    %Action{
      name: "Cały dom",
      function: type1,
      active: true,
      params: "{}",
      port: nil,
      args: sunblindsClose
    }
    |> Repo.insert!()

    # Salon - Swiatlo - 1 lampa id:2
    %Action{
      name: "Salon schody",
      function: type2,
      active: false,
      params: "[30000]",
      port_id: 18,
      start_time: ~T[15:00:00],
      end_time: ~T[05:00:00],
      args: [Repo.get(Port, 3)]
    }
    |> Repo.insert!()

    # KuchniaSalon - Swiatlo - 1 lampa id:3
    %Action{
      name: "Salon stół",
      function: type2,
      active: false,
      params: "[30000]",
      port_id: 19,
      start_time: ~T[15:00:00],
      end_time: ~T[05:00:00],
      args: [Repo.get(Port, 4)]
    }
    |> Repo.insert!()

    #    #Salon - Swiatlo - Dwa dimmery id:4
    %Action{
      name: "Salon wypoczynek",
      function: type2,
      active: false,
      params: "[30000]",
      port_id: 20,
      start_time: ~T[15:00:00],
      end_time: ~T[05:00:00],
      args: [Repo.get(Port, 6), Repo.get(Port, 7), Repo.get(Port, 9), Repo.get(Port, 10)]
    }
    |> Repo.insert!()

    # Default sunblinds openening id:5
    %Action{
      name: "Auto otwieranie",
      function: type1,
      active: true,
      params: "{}",
      port: nil,
      args: sunblindsOpen
    }
    |> Repo.insert!()


    # Przemek's room sunblinds openening id:6
    %Action{
      name: "Pokój Przemek",
      function: type1,
      active: true,
      params: "{}",
      port: nil,
      args: sunblindsPrzemek
    }
    |> Repo.insert!()

    :ok
  end

  def insert_tasks() do
    #    id: 1
    %TaskType{
      #      name: "Calling action up",
      name: "Akcja z wysokim sygnałem",
      module: "Core.Tasks.ExecuteActionUp",
      action: true,
      device: false,
    }
    |> Repo.insert!()

    #    id: 2
    %TaskType{
      #      name: "Calling action down",
      name: "Akcja z niskim sygnałem",
      module: "Core.Tasks.ExecuteActionDown",
      action: true,
      device: false,
    }
    |> Repo.insert!()

    #    id: 3
    %TaskType{
      name: "Read device inputs",
      name: "Odczytaj wejścia urządzenia",
      module: "Core.Tasks.ReadInputs",
      action: false,
      device: true,
    }
    |> Repo.insert!()

    #    id: 4
    %TaskType{
      #      name: "Heartbeat",
      name: "Nasłuchuj urządzenie",
      module: "Core.Tasks.Heartbeat",
      action: false,
      device: true,
    }
    |> Repo.insert!()

    #    id: 5
    %TaskType{
      #      name: "Read Wattmeters data",
      name: "Odczytaj watomierze urządzenia",
      module: "Core.Tasks.ReadUsedEnergy",
      action: false,
      device: true,
    }
    |> Repo.insert!()

    #    id: 6
    %TaskType{
      #      name: "Read Thermometers data",
      name: "Odczytaj termometry urządzenia",
      module: "Core.Tasks.ReadTemperature",
      action: false,
      device: true,
    }
    |> Repo.insert!()

    #    id: 7
    %TaskType{
      #      name: "Read Outputs",
      name: "Odczytaj wyjścia urządzenia",
      module: "Core.Tasks.ReadOutputs",
      action: false,
      device: true,
    }
    |> Repo.insert!()

    #    id: 8
    %TaskType{
      #      name: "Clean logs",
      name: "Wyczyść historię",
      module: "Core.Tasks.CleanLogs",
      action: false,
      device: false,
    }
    |> DB.Repo.insert!()
    # id: 9
    %TaskType{
      #      name: "Clean logs",
      name: "Zbierz logi w godzinne grupy",
      module: "Core.Tasks.CollectDataHourly",
      action: false,
      device: false,
    }
    |> DB.Repo.insert!()
    #    id: 9
    #    %TaskType{
    #      name: "Device activity supervisor task",
    #      module: "Core.Tasks.DeviceActivitySupervisor",
    #      action: false,
    #      device: true,
    #    }
    #    |> Repo.insert!()


    # read integra inputs id: 1
    %Task{
      type_id: 3,
      name: "Odczyt wejść Satel",
      status: "waiting",
      action: nil,
      device_id: 2,
      execution_time: nil,
      frequency: 1_000,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds closed at night id:2
    %Task{
      type_id: 1,
      name: "Zamykanie rolet",
      status: "waiting",
      action_id: 1,
      device: nil,
      frequency: 0,
      execution_time: ~T[19:00:00],
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds opened at morning id:3
    %Task{
      type_id: 2,
      name: "Otwieranie rolet",
      status: "waiting",
      action_id: 5,
      device: nil,
      frequency: 0,
      execution_time: ~T[06:00:00],
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds opened at morning id:4
    %Task{
      type_id: 4,
      name: "Nasłuchiwanie Arduino MEGA",
      status: "waiting",
      action: nil,
      device_id: 1,
      frequency: 10000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds opened at morning id:5
    %Task{
      type_id: 5,
      name: "Odczyt temperatury Arduino MEGA",
      status: "inactive",
      action: nil,
      device_id: 1,
      frequency: 300_000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds opened at morning id:6
    %Task{
      type_id: 6,
      name: "Odczyt watów Arduino MEGA",
      status: "inactive",
      action: nil,
      device_id: 1,
      frequency: 300_000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # clear logs id:7
    %Task{
      type_id: 8,
      name: "Czyszczenie logów z urządzeń",
      status: "inactive",
      action: nil,
      device_id: nil,
      frequency: 10_080_000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # read shelly output status id:8
    %Task{
      type_id: 7,
      name: "Odczyt stanu wyjść Shelly2",
      status: "inactive",
      action: nil,
      device_id: 1,
      frequency: 5_000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds opened at morning - Przemek's room id:9
    %Task{
      type_id: 2,
      name: "Otwieranie rolet Przemek",
      status: "waiting",
      action_id: 6,
      device: nil,
      frequency: 0,
      execution_time: ~T[06:00:00],
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()
    # id: 10
    %DB.Task{
      type_id: 9,
      name: "Grupwanie logów",
      status: "inactive",
      action: nil,
      device_id: nil,
      frequency: 3_600_000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> DB.Repo.insert!()

    :ok
  end

  def init_pages() do
    %Page{
      name: "Do testowania",
      order: 1,
      title: "",
      description: "",
      lights: [], #get_(Light, [1, 2, 3, 4, 5, 6]),
      ports: [],
      dimmers: [], #get_(Dimmer, [1, 2, 3]),
      sunblinds: [], #get_(Sunblind, [1, 2, 3, 4, 5, 6]),
      actions: [], #get_(Action, [1, 2, 3, 4]),
      tasks: [], #get_(Task, [1, 2, 3, 4, 5, 6]),
      devices: []
    }
    |> Repo.insert!()
    #
    #    %Page{
    #      name: "Wszystkie urządzenia",
    #      order: 1,
    #      title: "Przemek",
    #      description: "Page about Przemek's Room",
    #      lights: get_(Light, [1, 2, 3, 4, 5, 6]),
    #      ports: [],
    #      dimmers: get_(Dimmer, [1, 2, 3]),
    #      sunblinds: get_(Sunblind, [1, 2, 3, 4, 5, 6]),
    #      actions: get_(Action, [1, 2, 3, 4]),
    #      tasks: get_(Task, [1, 2, 3, 4, 5, 6]),
    #      devices: []
    #    }
    #    |> Repo.insert!()


    %Page{
      name: "Wszystkie zadania i akcje",
      order: 2,
      title: "",
      description: "",
      lights: [],
      ports: [],
      dimmers: [],
      sunblinds: [],
      actions: get_(Action, [1, 2, 3, 4, 5, 6]),
      tasks: get_(Task, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
      devices: []
    }
    |> Repo.insert!()

    %Page{
      name: "Wszystkie rolety",
      order: 3,
      title: "",
      description: "",
      lights: [],
      ports: [],
      dimmers: [],
      sunblinds: get_(Sunblind, [1, 2, 3, 4, 5, 6]),
      actions: [],
      tasks: [],
      devices: []
    }
    |> Repo.insert!()

    %Page{
      name: "Poddasze",
      order: 4,
      title: "",
      description: "",
      lights: [], #get_(Light, [1, 2, 3, 4, 5, 6]),
      ports: [],
      dimmers: get_(Dimmer, [4]),
      sunblinds: [], #get_(Sunblind, [5, 6]),
      actions: [], #get_(Action, [2, 3, 4]),
      tasks: [],
      devices: []
    }
    |> Repo.insert!()


    %Page{
      name: "Salon",
      order: 5,
      title: "",
      description: "",
      lights: [], # get_(Light, [1, 2, 3, 4, 5, 6]),
      ports: [],
      dimmers: get_(Dimmer, [1, 2, 3]),
      sunblinds: get_(Sunblind, [5, 6]),
      actions: get_(Action, [2, 3, 4]),
      tasks: [],
      devices: []
    }
    |> Repo.insert!()

    %Page{
      name: "Pokój Przemka",
      order: 5,
      title: "",
      description: "",
      lights: [], #get_(Light, [1, 2, 3, 4, 5, 6]),
      ports: [],
      dimmers: [], #get_(Dimmer, [1, 2, 3]),
      sunblinds: get_(Sunblind, [2, 3]),
      actions: get_(Action, [6]),
      tasks: get_(Task, [9]),
      devices: []
    }
    |> Repo.insert!()


    :ok
  end

  def init_buttons() do
    [
      %DB.RfButton{
        action_id: nil,
        key_value: "1198E8",
        mode: "toggle",
        name: "8365000A2C-1",
        port_id: 3,
        task_id: nil
      },
      %DB.RfButton{
        action_id: nil,
        key_value: "1198EC",
        mode: "toggle",
        name: "8365000A2C-2",
        port_id: 4,
        task_id: nil
      },
      %DB.RfButton{
        action_id: nil,
        key_value: "1198E4",
        mode: "toggle",
        name: "8365000A2C-3",
        port_id: 6,
        task_id: nil
      },
      %DB.RfButton{
        action_id: nil,
        key_value: "1198E9",
        mode: "toggle",
        name: "8365000A2C-4",
        port_id: 7,
        task_id: nil
      },
      %DB.RfButton{
        action_id: nil,
        key_value: "1198E2",
        mode: "toggle",
        name: "8365000A2C-5",
        port_id: 9,
        task_id: nil
      },
      %DB.RfButton{
        action_id: nil,
        key_value: "1198E5",
        mode: "toggle",
        name: "8365000A2C-6",
        port_id: 10,
        task_id: nil
      },
      %DB.RfButton{
        action_id: nil,
        key_value: "1198E1",
        mode: "toggle",
        name: "8365000A2C-7",
        port_id: nil,
        task_id: nil
      },
      %DB.RfButton{
        action_id: nil,
        key_value: "1198E3",
        mode: "toggle",
        name: "8365000A2C-8",
        port_id: nil,
        task_id: nil
      }
    ]
    |> Enum.each(&Repo.insert!/1)

    :ok
  end

  def init_therm() do
    #    id: 1
    %Thermometer{
      device_id: 1,
      name: "Test t1",
      address: "123",
      ref: 1
    }
    |> Repo.insert!()
    #    id: 2
    %Thermometer{
      device_id: 2,
      name: "Test t2",
      address: "124",
      ref: 1
    }
    |> Repo.insert!()


    %DB.Thermometer.Read{
      therm_id: 2,
      value: 20.0
    }
    |> Repo.insert!()

    %DB.Thermometer.Read{
      therm_id: 1,
      value: 11.0
    }
    |> Repo.insert!()

    %DB.Thermometer.Read{
      therm_id: 1,
      value: 10.5
    }
    |> Repo.insert!()

    %DB.Thermometer.Read{
      therm_id: 1,
      value: 11.0
    }
    |> Repo.insert!()

    %DB.Thermometer.Read{
      therm_id: 1,
      value: 12.0
    }
    |> Repo.insert!()
    :ok
  end

  defp get_(type, ids) do
    for i <- ids do
      Repo.get(type, i)
    end
  end
end
