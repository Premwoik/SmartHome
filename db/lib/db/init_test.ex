defmodule DB.InitTest do
  @moduledoc nil

  alias DB.Repo
  alias DB.Action
  alias DB.Device
  alias DB.Port
  alias DB.Dimmer
  alias DB.Watcher
  alias DB.Task
  alias DB.TaskType
  alias DB.Light
  alias DB.Sunblind
  import Ecto.Query
  require Logger

  def start_link, do: {:ok, spawn_link(__MODULE__, :run, [])}

  def run do
    Logger.info("Creating data! - TEST")
    :ok = delete_all()

    if !exist do
      :ok = insert_ard_mega()
      :ok = insert_integra64()
      #      :ok = insert_tasks()
      :ok
    end

    Logger.info("Data created!")
  end

  defp exist do
    length(Repo.all(Port)) > 0
  end

  def delete_all() do
    Repo.delete_all(Light)
    Repo.delete_all(Dimmer)
    Repo.delete_all(Watcher)
    Repo.delete_all(Action)
    Repo.delete_all(Port)
    Repo.delete_all(Device)
    Repo.delete_all(TaskType)
    Repo.delete_all(Task)
    Repo.delete_all(Sunblind)
    :ok
  end

  def insert_ard_mega(test \\ false) do
    # 1
    p2 = %Port{name: "mock", type: "undef", number: 2, mode: "undef", state: nil}
    # 2
    p18 = %Port{
      name: "Jadalnia",
      type: "dimmer",
      number: 18,
      mode: "output",
      timeout: 4_500,
      state: false
    }

    # 3
    p19 = %Port{name: "Schody", type: "light", number: 19, mode: "output", state: false}
    # 4
    p20 = %Port{name: "Stół", type: "light", number: 20, mode: "output", state: false}
    # 5
    p21 = %Port{
      name: "Kanapa",
      type: "dimmer",
      number: 21,
      mode: "output",
      timeout: 4_500,
      state: false
    }

    # 6
    p22 = %Port{name: "Lewa", type: "light", number: 22, mode: "output", state: false}
    # 7
    p23 = %Port{name: "Prawa", type: "light", number: 23, mode: "output", state: false}
    # 8
    p24 = %Port{
      name: "Tv",
      type: "dimmer",
      number: 24,
      mode: "output",
      timeout: 4_500,
      state: false
    }

    # 9
    p25 = %Port{name: "Lewa", type: "light", number: 25, mode: "output", state: false}
    # 10
    p26 = %Port{name: "Prawa", type: "light", number: 26, mode: "output", state: false}
    # 11
    p34 = %Port{
      name: "Sypialnia",
      type: "sunblind",
      number: 34,
      mode: "output",
      timeout: 1000,
      state: false
    }

    # 12
    p35 = %Port{
      name: "Dzienny",
      type: "sunblind",
      number: 35,
      mode: "output",
      timeout: 1000,
      state: false
    }

    # 13
    p36 = %Port{name: "TarasP", type: "sunblind", number: 36, mode: "output", state: false}
    # 14
    p37 = %Port{name: "BalkonP", type: "sunblind", number: 37, mode: "output", state: false}
    # 15
    p38 = %Port{name: "Michał", type: "sunblind", number: 38, mode: "output", state: false}
    # 16
    p39 = %Port{name: "Salon", type: "sunblind", number: 39, mode: "output", state: false}
    # 17
    p40 = %Port{name: "TarasSalon", type: "sunblind", number: 40, mode: "output", state: false}

    type = if test, do: "Core.DeviceMock", else: "Core.Device.Default"

    dev1 =
      %Device{
        name: "ard_mega",
        ip: "192.168.2.137",
        port: 1000,
        type: type,
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

    %Sunblind{port_id: 11, full_open_time: 10_000, type: "pulse"}
    |> Repo.insert!()

    %Sunblind{port_id: 12, full_open_time: 10_000, type: "pulse"}
    |> Repo.insert!()

    %Sunblind{port_id: 13, full_open_time: 10_000}
    |> Repo.insert!()

    %Sunblind{port_id: 14, full_open_time: 10_000}
    |> Repo.insert!()

    %Sunblind{port_id: 15, full_open_time: 10_000}
    |> Repo.insert!()

    %Sunblind{port_id: 16, full_open_time: 10_000}
    |> Repo.insert!()

    %Sunblind{port_id: 17, full_open_time: 10_000}
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

    :ok
  end

  def insert_integra64(test \\ false) do
    # 18
    p8 = %Port{
      name: "CzujkaSalonSchody",
      type: "motion_sensor",
      number: 8,
      mode: "undef",
      state: nil
    }

    # 19
    p9 = %Port{
      name: "CzujkaSalonKuchnia",
      type: "motion_sensor",
      number: 9,
      mode: "undef",
      state: nil
    }

    # 20
    p10 = %Port{name: "CzujkaSalon", type: "motion_sensor", number: 10, mode: "undef", state: nil}
    # 21
    p62 = %Port{
      name: "SygnalZamknieciaRolet",
      type: "alarm_input",
      number: 62,
      mode: "undef",
      state: nil
    }

    type = if test, do: "Core.DeviceMock", else: "Core.Device.Satel"

    dev1 =
      %Device{
        name: "integra",
        ip: "192.168.2.136",
        port: 9000,
        type: type,
        ports: [p8, p9, p10, p62]
      }
      |> Repo.insert!()

    sunblids = Repo.all(from(p in Port, where: p.id in [11, 12, 13, 14, 15, 16]))

    type1 = if test, do: "ActionMock", else: "CloseSunblinds"
    type2 = if test, do: "ActionMock", else: "AutoLights"
    # CałyDom - RoletyZamykanie
    %Action{
      function: type1,
      active: false,
      params: "{}",
      port_id: 21,
      args: sunblids
    }
    |> Repo.insert!()

    # Salon - Swiatlo - 1 lampa
    %Action{
      function: type2,
      active: true,
      params: "[30000]",
      port_id: 18,
      start_time: ~T[06:00:00],
      end_time: ~T[18:00:00],
      args: [Repo.get(Port, 3)]
    }
    |> Repo.insert!()

    # KuchniaSalon - Swiatlo - 1 lampa
    %Action{
      function: type1,
      active: true,
      params: "[30000]",
      port_id: 19,
      args: [Repo.get(Port, 4)]
    }
    |> Repo.insert!()

    #    #Salon - Swiatlo - Dwa dimmery
    %Action{
      function: type1,
      active: true,
      params: "[30000]",
      port_id: 20,
      args: [Repo.get(Port, 6), Repo.get(Port, 7), Repo.get(Port, 9), Repo.get(Port, 10)]
    }
    |> Repo.insert!()

    #    #Test1
    %Action{
      function: type1,
      active: true,
      params: "[30000]",
      port_id: 1,
      start_time: ~T[18:00:00],
      end_time: ~T[06:00:00],
      args: []
    }
    |> Repo.insert!()

    %Watcher{device_id: 2, status: true, freq: 1000}
    |> Repo.insert!()

    :ok
  end

  def insert_mock_device(test \\ false) do
    # 22
    p8 = %Port{name: "M1", type: "light", number: 8, mode: "undef", state: nil}
    # 23
    p9 = %Port{name: "M2", type: "light", number: 9, mode: "undef", state: nil}
    # 24
    p10 = %Port{name: "M3", type: "light", number: 10, mode: "undef", state: nil}
    # 25
    p62 = %Port{name: "M4", type: "light", number: 62, mode: "undef", state: nil}

    dev1 =
      %Device{
        name: "mock_dev",
        ip: "192.168.2.136",
        port: 9000,
        type: "Core.DeviceMock",
        ports: [p8, p9, p10, p62]
      }
      |> Repo.insert!()
  end

  def insert_tasks(test \\ false) do
    %TaskType{
      name: "Calling action",
      module: "Core.Tasks.TaskMock"
    }
    |> Repo.insert!()

    %TaskType{
      name: "Read device inputs",
      module: "Core.Tasks.TaskMock"
    }
    |> Repo.insert!()

    # read integra inputs
    %Task{
      type_id: 1,
      status: "waiting",
      action_id: nil,
      device_id: nil,
      frequency: 1,
      limit: 3,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!()

    # make sunblinds closed at night
    %Task{
      type_id: 1,
      status: "inactive",
      action: nil,
      device: nil,
      frequency: 1,
      limit: 3,
      start_date: ~N[2018-10-18 18:00:00],
      end_date: ~N[2018-10-18 19:00:00]
    }
    |> Repo.insert!()

    :ok
  end
end
