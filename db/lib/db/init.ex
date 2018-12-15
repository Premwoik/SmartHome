defmodule DB.Init do
  @moduledoc nil

  alias DB.{Light, Task, TaskType, Watcher, Dimmer, Port, Device, Action, Repo, Sunblind, Page, PageContent}
  import Ecto.Query
  require Logger


  def start_link, do:
    {:ok, spawn_link(__MODULE__, :run, [])}

  def run do
    Logger.info("Creating data!")
    :ok = delete_all()
    if !exist? do
      :ok = insert_ard_mega()
      :ok = insert_integra64()
      :ok = insert_tasks()
      :ok = init_pages
      :ok
    end
    Logger.info("Data created!")
  end

  defp exist? do
    (length Repo.all Port) > 0
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

  def insert_ard_mega() do
    p2 = %Port{name: "mock", type: "undef", number: 2, mode: "undef", state: nil} #1
    p18 = %Port{name: "Jadalnia", type: "dimmer", number: 18, mode: "output", timeout: 4_500, state: false}#2
    p19 = %Port{name: "Schody", type: "light", number: 19, mode: "output", state: false}#3
    p20 = %Port{name: "Stół", type: "light", number: 20, mode: "output", state: false}#4
    p21 = %Port{name: "Kanapa", type: "dimmer", number: 21, mode: "output", timeout: 4_500, state: false}#5
    p22 = %Port{name: "Lewa", type: "light", number: 22, mode: "output", state: false}#6
    p23 = %Port{name: "Prawa", type: "light", number: 23, mode: "output", state: false}#7
    p24 = %Port{name: "Tv", type: "dimmer", number: 24, mode: "output", timeout: 4_500, state: false}#8
    p25 = %Port{name: "Lewa", type: "light", number: 25, mode: "output", state: false}#9
    p26 = %Port{name: "Prawa", type: "light", number: 26, mode: "output", state: false}#10
    p34 = %Port{name: "Sypialnia", type: "sunblind", number: 34, mode: "output", timeout: 1000, state: false}#11
    p35 = %Port{name: "Dzienny", type: "sunblind", number: 35, mode: "output", timeout: 1000, state: false}#12
    p36 = %Port{name: "TarasP", type: "sunblind", number: 36, mode: "output", state: false}#13
    p37 = %Port{name: "BalkonP", type: "sunblind", number: 37, mode: "output", state: false}#14
    p38 = %Port{name: "Michał", type: "sunblind", number: 38, mode: "output", state: false}#15
    p39 = %Port{name: "Salon", type: "sunblind", number: 39, mode: "output", state: false}#16
    p40 = %Port{name: "TarasSalon", type: "sunblind", number: 40, mode: "output", state: false}#17


    dev1 = %Device{
             name: "ard_mega",
             ip: "192.168.2.137",
             port: 1000,
             type: "Core.Device.Default",
             ports: [p2, p18, p19, p20,
               p21, p22, p23, p24, p25, p26, p34, p35, p36, p37, p38, p39, p40]
           }
           |> Repo.insert!


    %Sunblind{port_id: 11, type: "pulse", full_open_time: 16_000}
    |> Repo.insert!
    %Sunblind{port_id: 12, type: "pulse", full_open_time: 16_000}
    |> Repo.insert!

    %Sunblind{port_id: 13, full_open_time: 30_000}
    |> Repo.insert!

    %Sunblind{port_id: 14, full_open_time: 30_000}
    |> Repo.insert!

    %Sunblind{port_id: 15, full_open_time: 30_000}
    |> Repo.insert!

    %Sunblind{port_id: 16, full_open_time: 30_000}
    |> Repo.insert!

    %Sunblind{port_id: 17, full_open_time: 30_000}
    |> Repo.insert!



    l1 = %Light{port_id: 3, dimmer_id: 2} #1
    l2 = %Light{port_id: 4, dimmer_id: 2} #2
    l3 = %Light{port_id: 6, dimmer_id: 5} #3
    l4 = %Light{port_id: 7, dimmer_id: 5} #4
    l5 = %Light{port_id: 9, dimmer_id: 8} #5
    l6 = %Light{port_id: 10, dimmer_id: 8} #6


    %Dimmer{port_id: 2, fill: 0, lights: [l1, l2]}
    |> Repo.insert!
    %Dimmer{port_id: 5, fill: 0, lights: [l3, l4]}
    |> Repo.insert!
    %Dimmer{port_id: 8, fill: 0, lights: [l5, l6]}
    |> Repo.insert!

    :ok
  end


  def insert_integra64(test \\ false) do
    p8 = %Port{name: "CzujkaSalonSchody", type: "motion_sensor", number: 8, mode: "undef", state: nil}#19
    p9 = %Port{name: "CzujkaSalonKuchnia", type: "motion_sensor", number: 9, mode: "undef", state: nil}#20
    p10 = %Port{name: "CzujkaSalon", type: "motion_sensor", number: 10, mode: "undef", state: nil}#21
    p62 = %Port{name: "SygnalZamknieciaRolet", type: "alarm_input", number: 62, mode: "undef", state: nil}#22

    dev1 = %Device{
             name: "integra",
             ip: "192.168.2.136",
             port: 9000,
             type: "Core.Device.Satel",
             process: false,
             ports: [p8, p9, p10, p62]
           }
           |> Repo.insert!


    sunblinds = Repo.all from p in Port, where: p.id in [11, 12, 13, 14, 15, 16]


    type1 = "CloseSunblinds"
    type2 = "AutoLights"
    #CałyDom - RoletyZamykanie
    %Action{
      name: "Zamykanie rolet",
      function: type1,
      active: true,
      params: "{}",
      port: nil,
      args: sunblinds
    }
    |> Repo.insert!

    #Salon - Swiatlo - 1 lampa
    %Action{
      name: "Salon schody",
      function: type2,
      active: true,
      params: "[30000]",
      port_id: 18,
      start_time: ~T[16:00:00],
      end_time: ~T[05:00:00],
      args: [Repo.get(Port, 3)]
    }
    |> Repo.insert!


    #KuchniaSalon - Swiatlo - 1 lampa
    %Action{
      name: "Salon stół",
      function: type2,
      active: true,
      params: "[30000]",
      port_id: 19,
      start_time: ~T[16:00:00],
      end_time: ~T[05:00:00],
      args: [Repo.get(Port, 4)]
    }
    |> Repo.insert!

    #    #Salon - Swiatlo - Dwa dimmery
    %Action{
      name: "Salon wypoczynek",
      function: type2,
      active: true,
      params: "[30000]",
      port_id: 20,
      start_time: ~T[16:00:00],
      end_time: ~T[05:00:00],
      args: [Repo.get(Port, 6), Repo.get(Port, 7), Repo.get(Port, 9), Repo.get(Port, 10)]
    }
    |> Repo.insert!


    :ok
  end

  def insert_tasks() do
    %TaskType{
      name: "Calling action up",
      module: "Core.Tasks.ExecuteActionUp"
    }
    |> Repo.insert!

    %TaskType{
      name: "Calling action down",
      module: "Core.Tasks.ExecuteActionDown"
    }
    |> Repo.insert!

    %TaskType{
      name: "Read device inputs",
      module: "Core.Tasks.ReadInputs"
    }
    |> Repo.insert!
    %TaskType{
      name: "Heartbeat",
      module: "Core.Tasks.Heartbeat"
    }
    |> Repo.insert!
    %TaskType{
      name: "Read Wattmeters data",
      module: "Core.Tasks.ReadUsedEnergy"
    }
    |> Repo.insert!
    %TaskType{
      name: "Read Thermometers data",
      module: "Core.Tasks.ReadTemperature"
    }
    |> Repo.insert!


    # read integra inputs
    %Task{
      type_id: 3,
      name: "Czytaj satel",
      status: "inactive",
      action: nil,
      device_id: 2,
      execution_time: nil,
      frequency: 1_000,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!

    # make sunblinds closed at night
    %Task{
      type_id: 1,
      name: "Zamykanie rolet",
      status: "inactive",
      action_id: 1,
      device: nil,
      frequency: 0,
      execution_time: ~T[16:02:00],
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!

    # make sunblinds opened at morning
    %Task{
      type_id: 2,
      name: "Otwieranie rolet",
      status: "inactive",
      action_id: 1,
      device: nil,
      frequency: 0,
      execution_time: ~T[06:00:00],
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!

    # make sunblinds opened at morning
    %Task{
      type_id: 4,
      name: "heartbeat arduino mega",
      status: "waiting",
      action: nil,
      device_id: 1,
      frequency: 5000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!

    # make sunblinds opened at morning
    %Task{
      type_id: 5,
      name: "read temps from arduino mega",
      status: "waiting",
      action: nil,
      device_id: 1,
      frequency: 5000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!

    # make sunblinds opened at morning
    %Task{
      type_id: 6,
      name: "read watts from arduino mega",
      status: "waiting",
      action: nil,
      device_id: 1,
      frequency: 5000,
      execution_time: nil,
      limit: -1,
      start_date: nil,
      end_date: nil
    }
    |> Repo.insert!




    :ok
  end


  def init_pages() do
   %Page{
      name: "Rooms",
      order: 1,
      title: "Przemek",
      description: "Page about Przemek's Room",
      lights: [Repo.get(Light, 1), Repo.get(Light, 3)],
      ports: [Repo.get(Port, 2), Repo.get(Port, 5)],
      dimmers: [Repo.get(Dimmer, 2), Repo.get(Dimmer, 3)],
      sunblinds: [Repo.get(Sunblind, 1)],
      actions: [Repo.get(Action, 1), Repo.get(Action, 2)],
      tasks: []
    }
    |> Repo.insert!
    :ok

  end

end
