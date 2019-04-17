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
    PageContent
  }

  import Ecto.Query
  require Logger

  def start_link, do: {:ok, spawn_link(__MODULE__, :run, [])}

  def run do
    Logger.info("Creating data!")
    :ok = delete_all()

    if !exist? do
      :ok = insert_ard_mega()
      :ok = insert_integra64()
      :ok = insert_tasks()
      :ok = init_pages()
      :ok = insert_shelly()
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

  def insert_shelly() do

    type = %DeviceType{
      name: "Shelly",
      module: "Core.Device.Shelly"
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

    dev1 =
      %Device{
        name: "shelly1",
        ip: "192.168.2.138",
        port: 80,
        alive: true,
        process: false,
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
        process: false,
        type_id: type.id,
        ports: [p2]
      }
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

    type = %DeviceType{
      name: "Default",
      module: "Core.Device.Default"
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

    %Dimmer{port_id: 2, type: "click", fill: 0, lights: [l1, l2]}
    |> Repo.insert!()

    %Dimmer{port_id: 5, type: "click", fill: 0, lights: [l3, l4]}
    |> Repo.insert!()

    %Dimmer{port_id: 8, type: "click", fill: 0, lights: [l5, l6]}
    |> Repo.insert!()

    :ok
  end

  def insert_integra64(test \\ false) do
    # 19
    p8 = %Port{
      name: "CzujkaSalonSchody",
      type: "motion_sensor",
      number: 8,
      mode: "undef",
      state: nil
    }

    # 20
    p9 = %Port{
      name: "CzujkaSalonKuchnia",
      type: "motion_sensor",
      number: 9,
      mode: "undef",
      state: nil
    }

    # 21
    p10 = %Port{name: "CzujkaSalon", type: "motion_sensor", number: 10, mode: "undef", state: nil}
    # 22
    p62 = %Port{
      name: "SygnalZamknieciaRolet",
      type: "alarm_input",
      number: 62,
      mode: "undef",
      state: nil
    }

   type = %DeviceType{
      name: "Satel",
      module: "Core.Device.Satel"
    }
    |> Repo.insert!()



    dev1 =
      %Device{
        name: "integra",
        ip: "192.168.2.136",
        port: 9000,
        type_id: type.id,
        alive: true,
        process: true,
        ports: [p8, p9, p10, p62]
      }
      |> Repo.insert!()

    sunblinds = Repo.all(from(p in Port, where: p.id in [12, 13, 14, 15, 16, 17]))

    type1 = "CloseSunblinds"
    type2 = "AutoLights"
    # CałyDom - RoletyZamykanie
    %Action{
      name: "Zamykanie rolet",
      function: type1,
      active: true,
      params: "{}",
      port: nil,
      args: sunblinds
    }
    |> Repo.insert!()

    # Salon - Swiatlo - 1 lampa
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

    # KuchniaSalon - Swiatlo - 1 lampa
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

    #    #Salon - Swiatlo - Dwa dimmery
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

    :ok
  end

  def insert_tasks() do
    %TaskType{
      name: "Calling action up",
      module: "Core.Tasks.ExecuteActionUp"
    }
    |> Repo.insert!()

    %TaskType{
      name: "Calling action down",
      module: "Core.Tasks.ExecuteActionDown"
    }
    |> Repo.insert!()

    %TaskType{
      name: "Read device inputs",
      module: "Core.Tasks.ReadInputs"
    }
    |> Repo.insert!()

    %TaskType{
      name: "Heartbeat",
      module: "Core.Tasks.Heartbeat"
    }
    |> Repo.insert!()

    %TaskType{
      name: "Read Wattmeters data",
      module: "Core.Tasks.ReadUsedEnergy"
    }
    |> Repo.insert!()

    %TaskType{
      name: "Read Thermometers data",
      module: "Core.Tasks.ReadTemperature"
    }
    |> Repo.insert!()

    # read integra inputs
    %Task{
      type_id: 3,
      name: "Czytaj satel",
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

    # make sunblinds closed at night
    %Task{
      type_id: 1,
      name: "Zamykanie rolet",
      status: "waiting",
      action_id: 1,
      device: nil,
      frequency: 0,
      execution_time: ~T[16:00:00],
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds opened at morning
    %Task{
      type_id: 2,
      name: "Otwieranie rolet",
      status: "waiting",
      action_id: 1,
      device: nil,
      frequency: 0,
      execution_time: ~T[06:00:00],
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds opened at morning
    %Task{
      type_id: 4,
      name: "heartbeat arduino mega",
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

    # make sunblinds opened at morning
    %Task{
      type_id: 5,
      name: "read temps from arduino mega",
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

    # make sunblinds opened at morning
    %Task{
      type_id: 6,
      name: "read watts from arduino mega",
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

    :ok
  end

  def init_pages() do
    %Page{
      name: "Wszystkie urządzenia",
      order: 1,
      title: "Przemek",
      description: "Page about Przemek's Room",
      lights: get_(Light, [1, 2, 3, 4, 5, 6]),
      ports: [],
      dimmers: get_(Dimmer, [1, 2, 3]),
      sunblinds: get_(Sunblind, [1, 2, 3, 4, 5, 6]),
      actions: get_(Action, [1, 2, 3, 4]),
      tasks: get_(Task, [1, 2, 3, 4, 5, 6]),
      devices: []

    }
    |> Repo.insert!()

    %Page{
      name: "Salon",
      order: 1,
      title: "Przemek",
      description: "Page about Przemek's Room",
      lights: get_(Light, [1, 2, 3, 4, 5, 6]),
      ports: [],
      dimmers: get_(Dimmer, [1, 2, 3]),
      sunblinds: get_(Sunblind, [5, 6]),
      actions: get_(Action, [2, 3, 4]),
      tasks: [],
      devices: []
    }
    |> Repo.insert!()

    %Page{
      name: "Zadania i akcje",
      order: 1,
      title: "Przemek",
      description: "Page about Przemek's Room",
      lights: [],
      ports: [],
      dimmers: [],
      sunblinds: [],
      actions: get_(Action, [1, 2, 3, 4]),
      tasks: get_(Task, [1, 2, 3, 4, 5, 6]),
      devices: []
    }
    |> Repo.insert!()

    %Page{
      name: "Rolety",
      order: 1,
      title: "Przemek",
      description: "Page about Przemek's Room",
      lights: [],
      ports: [],
      dimmers: [],
      sunblinds: get_(Sunblind, [1, 2, 3, 4, 5, 6]),
      actions: [],
      tasks: [],
      devices: []
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
