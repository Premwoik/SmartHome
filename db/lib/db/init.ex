defmodule DB.Init do
  @moduledoc nil

  alias DB.Repo
  alias DB.Action
  alias DB.Device
  alias DB.Port
  alias DB.Dimmer
  alias DB.Watcher
  import Ecto.Query
  require Logger


  def start_link, do:
    {:ok, spawn_link(__MODULE__, :run, [])}

  def run do
    Logger.info("Creating data!")
#    :ok = delete_all()
    :ok = insert_ard_mega()
    :ok = insert_integra64()
    Logger.info("Data created!")
  end


  defp delete_all() do
    Repo.delete_all(Action)
    Repo.delete_all(Watcher)
    Repo.delete_all(Dimmer)
    Repo.delete_all(Port)
    Repo.delete_all(Device)
    :ok
  end

  def insert_ard_mega do
    p2 = %Port{name: "mock", type: "undef", number: 2, mode: "undef", state: nil} #1
    p18 = %Port{name: "Jadalnia", type: "dimmer", number: 18, mode: "output", state: false}#2
    p19 = %Port{name: "Schody", type: "light", number: 19, mode: "output", state: false}#3
    p20 = %Port{name: "Stół", type: "light", number: 20, mode: "output", state: false}#4
    p21 = %Port{name: "Kanapa", type: "dimmer", number: 21, mode: "output", state: false}#5
    p22 = %Port{name: "Lewa", type: "light", number: 22, mode: "output", state: false}#6
    p23 = %Port{name: "Prawa", type: "light", number: 23, mode: "output", state: false}#7
    p24 = %Port{name: "Tv", type: "dimmer", number: 24, mode: "output", state: false}#8
    p25 = %Port{name: "Lewa", type: "light", number: 25, mode: "output", state: false}#9
    p26 = %Port{name: "Prawa", type: "light", number: 26, mode: "output", state: false}#10
    p34 = %Port{name: "Sypialnia", type: "sunblind", number: 34, mode: "output", timeout: 1000, state: true}#11
    p35 = %Port{name: "Dzienny", type: "sunblind", number: 35, mode: "output", timeout: 1000, state: true}#12
    p36 = %Port{name: "TarasP", type: "sunblind", number: 36, mode: "output", state: false}#13
    p37 = %Port{name: "BalkonP", type: "sunblind", number: 37, mode: "output", state: false}#14
    p38 = %Port{name: "Michał", type: "sunblind", number: 38, mode: "output", state: false}#16
    p39 = %Port{name: "Salon", type: "sunblind", number: 39, mode: "output", state: false}#17
    p40 = %Port{name: "TarasSalon", type: "sunblind", number: 40, mode: "output", state: false}#18


    dev1 = %Device{
             name: "ard_mega",
             ip: "192.168.2.137",
             port: 1000,
             type: "Device.Client",
             ports: [p2, p18, p19, p20,
               p21, p22, p23, p24, p25, p26, p34, p35, p36, p37, p38, p39, p40]
           }
           |> Repo.insert!


    %Dimmer{id: 2, fill: 0, lights: [Repo.get(Port,3), Repo.get(Port,4)]}
    |> Repo.insert!
    %Dimmer{id: 5, fill: 0, lights: [Repo.get(Port,6), Repo.get(Port,7)]}
    |> Repo.insert!
    %Dimmer{id: 8, fill: 0, lights: [Repo.get(Port,9), Repo.get(Port,10)]}
    |> Repo.insert!


    :ok
  end


  def insert_integra64 do
    p8 = %Port{name: "CzujkaSalonSchody", type: "motion_sensor", number: 8, mode: "undef", state: nil}#19
    p9 = %Port{name: "CzujkaSalonKuchnia", type: "motion_sensor", number: 9, mode: "undef", state: nil}#20
    p10 = %Port{name: "CzujkaSalon", type: "motion_sensor", number: 10, mode: "undef", state: nil}#21
    p62 = %Port{name: "SygnalZamknieciaRolet", type: "alarm_input", number: 62, mode: "undef", state: nil}#22



    dev1 = %Device{
             name: "integra",
             ip: "192.168.2.136",
             port: 9000,
             type: "Alarm.Satel.Integra64",
             ports: [p8, p9, p10, p62]
           }
           |> Repo.insert!


    sunblids = Repo.all from p in Port, where: p.id in [11,12,13,14,15,16]

    #CałyDom - RoletyZamykanie
    %Action{
      function: "close_sunblinds",
      active: true,
      params: "{}",
      port_id: 21,
      args: sunblids
    }
    |> Repo.insert!

    #Salon - Swiatlo - 1 lampa
    %Action{
      function: "turn_lights_on",
      active: false,
      params: "[30000]",
      port_id: 18,
      args: [Repo.get(Port,3)]
    }
    |> Repo.insert!


    #KuchniaSalon - Swiatlo - 1 lampa
    %Action{
      function: "turn_lights_on",
      active: true,
      params: "[30000]",
      port_id: 19,
      args: [Repo.get(Port,4)]
    }
    |> Repo.insert!

#    #Salon - Swiatlo - Dwa dimmery
    %Action{
      function: "turn_lights_on",
      active: true,
      params: "[30000]",
      port_id: 20,
      args: [Repo.get(Port,6), Repo.get(Port,7), Repo.get(Port,9), Repo.get(Port,10)]
    }
    |> Repo.insert!

    %Watcher{device_id: 2, status: true, freq: 1000}
    |> Repo.insert!

    :ok
  end

end
