defmodule DB.Init do
  @moduledoc false

  alias DB.{Port, Action, Device, ScheduleJob, Meter, RfButton, Page}

  @spec setup!(nodes :: list(node)) :: :ok
  def setup!(nodes \\ [node()]) do
    # Create the DB directory (if custom path given)
    if path = Application.get_env(:mnesia, :dir) do
      :ok = File.mkdir_p!(path)
    end

    # Create the Schema
    Memento.stop()
    Memento.Schema.create(nodes)
    Memento.start()

    create_tables(nodes)
    run_init()
  end

  def create_tables(nodes \\ [node()]) do
    Memento.Table.create!(DB.Device, disc_copies: nodes)
    Memento.Table.create!(DB.Port, disc_copies: nodes)
    Memento.Table.create!(DB.Action, disc_copies: nodes)
    Memento.Table.create!(DB.ScheduleJob, disc_copies: nodes)
    Memento.Table.create!(DB.RfButton, disc_copies: nodes)
    Memento.Table.create!(DB.Meter, disc_copies: nodes)
    Memento.Table.create!(DB.Page, disc_copies: nodes)
  end

  def run_init() do
    Enum.map(devices(), fn d -> Device.insert(d) end)
    Enum.map(ports(), fn d -> Device.insert(d) end)
    Enum.map(actions(), fn d -> Device.insert(d) end)
    Enum.map(jobs(), fn d -> Device.insert(d) end)
    Enum.map(rf_buttons(), fn d -> Device.insert(d) end)
    Enum.map(meters(), fn d -> Device.insert(d) end)
    Enum.map(pages(), fn d -> Page.insert(d) end)
  end

  def devices do
    [
      Device.new(name: "ard_mega", ip: "192.168.2.137", port: 1000, type: "Default"),
      Device.new(name: "integra", ip: "192.168.2.136", port: 9000, type: "Satel"),
      Device.new(name: "shelly1-59D6AF", ip: "192.168.2.207", type: "Shelly"),
      Device.new(name: "shelly1-246E88", ip: "192.168.2.208", type: "Shelly"),
      Device.new(name: "shellyRGBW2", ip: "192.168.2.210", type: "ShellyRGBW2"),
      Device.new(name: "Sonoff Rf Bridge", ip: "192.168.2.122", type: "SonoffRfBridge"),
      Device.new(name: "Kinkiet1-5EB49C", ip: "192.168.2.205", type: "SonoffBasic"),
      Device.new(name: "Kinkiet2-EA3266", ip: "192.168.2.206", type: "SonoffBasic"),
      Device.new(name: "Zasilanie led", ip: "192.168.2.209", type: "SonoffBasic")
    ]
  end

  def ports do
    [
      Port.new(
        device_id: foreign(Device, 1),
        name: "Jadalnia",
        type: :dimmer,
        number: 18,
        mode: :output,
        timeout: 4500,
        inverted_logic: true,
        more: %{fill: 0, direction: 1, time: 4500}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Schody",
        type: :light,
        number: 19,
        mode: :output,
        inverted_logic: true,
        more: %{dimmer_id: foreign(Port, 1)}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Stół",
        type: :light,
        number: 20,
        mode: :output,
        inverted_logic: true,
        more: %{dimmer_id: foreign(Port, 1)}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Kanapa",
        type: :dimmer,
        number: 21,
        mode: :output,
        timeout: 100,
        inverted_logic: true,
        more: %{fill: 0, direction: 1, time: 4500}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Kanapa L",
        type: :light,
        number: 22,
        mode: :output,
        more: %{dimmer_id: foreign(Port, 4)},
        inverted_logic: true
      ),
      Port.new(
        device_id: foreign(Device,1),
        name: "Kanapa P",
        type: :light,
        number: 23,
        mode: :output,
        more: %{dimmer_id: foreign(Port, 4)},
        inverted_logic: true
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "TV",
        type: :dimmer,
        number: 24,
        mode: :output,
        timeout: 100,
        state: false,
        inverted_logic: true,
        more: %{fill: 0, direction: 1, time: 4500}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "TV L",
        type: :light,
        number: 25,
        mode: :output,
        more: %{dimmer_id: foreign(Port, 7)},
        inverted_logic: true
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "TV P",
        type: :light,
        number: 26,
        mode: :output,
        more: %{dimmer_id: foreign(Port, 7)},
        inverted_logic: true
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Parter open",
        type: :sunblind_helper,
        number: 34,
        more: [close_port_id: {:foreign, DB.Port, 11}],
        mode: :output_pulse,
        timeout: 1000,
        inverted_logic: true
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Parter close",
        type: :sunblind,
        number: 35,
        mode: :output_pulse,
        timeout: 1000,
        inverted_logic: true,
        more: %{
          type: :pulse2,
          full_open_time: 16000,
          state: :open,
          open_port_id: foreign(Port, 10)
        }
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Taras Przemek",
        type: :sunblind,
        number: 36,
        mode: :output,
        inverted_logic: true,
        more: %{type: :only_close, full_open_time: 30000, state: :open}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Balkon Przemek",
        type: :sunblind,
        number: 37,
        mode: :output,
        inverted_logic: true,
        more: %{type: :only_close, full_open_time: 30000, state: :open}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Balkon Michał",
        type: :sunblind,
        number: 38,
        mode: :output,
        inverted_logic: true,
        more: %{type: :only_close, full_open_time: 30000, state: :open}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Salon",
        type: :sunblind,
        number: 39,
        mode: :output,
        inverted_logic: true,
        more: %{type: :only_close, full_open_time: 30000, state: :open}
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Taras Salon",
        type: :sunblind,
        number: 40,
        mode: :output,
        inverted_logic: true,
        more: %{type: :only_close, full_open_time: 30000, state: :open}
      ),
      Port.new(
        device_id: foreign(Device, 6),
        name: "RF nieprzypisany",
        type: :light,
        number: 1,
        mode: :output,
        inverted_logic: true
      ),
      Port.new(
        device_id: foreign(Device, 6),
        name: "Salon w rogu",
        type: :light,
        number: 3,
        mode: :output,
        inverted_logic: true
      ),
      Port.new(
        device_id: foreign(Device, 6),
        name: "Salon komoda",
        type: :light,
        number: 5,
        mode: :output,
        inverted_logic: true
      ),
      Port.new(
        device_id: foreign(Device, 6),
        name: "Strych lampa stojąca",
        type: :light,
        number: 7,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 6),
        name: "RF nieprzypisany",
        type: :light,
        number: 9,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 6),
        name: "RF nieprzypisany",
        type: :light,
        number: 11,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 6),
        name: "Sypialnia",
        type: :light,
        number: 13,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 1),
        name: "Wiatrak łazienka",
        type: :port,
        number: 15,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 4),
        name: "Poddasze Główne",
        type: :light,
        number: 0,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 3),
        name: "Poddasze Gablota",
        type: :light,
        number: 0,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 8),
        name: "Kinkiet od okna",
        type: :light,
        number: 0,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 7),
        name: "Kinkiet",
        type: :light,
        number: 0,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 5),
        name: "Poddasze ledy belka",
        type: :dimmer_rgbw,
        number: 0,
        mode: :output,
        more: %{fill: 0, red: 255, green: 255, blue: 255, white: 255}
      ),
      Port.new(
        device_id: foreign(Device, 9),
        name: "Zasilanie led",
        type: :light,
        number: 0,
        mode: :output
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Sygnał ciepła woda",
        type: :alarm_input,
        number: 60,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Sygnał zamykanie rolet",
        type: :alarm_input,
        number: 62,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Piwnica przedpokój",
        type: :motion_sensor,
        number: 1,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Piwnica",
        type: :motion_sensor,
        number: 2,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Piwnica kuchnia",
        type: :motion_sensor,
        number: 3,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Piwnica kotłownia",
        type: :motion_sensor,
        number: 4,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Parter łazienka",
        type: :motion_sensor,
        number: 5,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Parter sypialnia",
        type: :motion_sensor,
        number: 6,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Parter",
        type: :motion_sensor,
        number: 7,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Salon schody",
        type: :motion_sensor,
        number: 8,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Salon stół",
        type: :motion_sensor,
        number: 9,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Salon",
        type: :motion_sensor,
        number: 10,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Salon kuchnia",
        type: :motion_sensor,
        number: 11,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Salon przedpokój",
        type: :motion_sensor,
        number: 12,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Piętro Przemek",
        type: :motion_sensor,
        number: 13,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Piętro Michał",
        type: :motion_sensor,
        number: 14,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Piętro łazienka",
        type: :motion_sensor,
        number: 15,
        mode: :input
      ),
      Port.new(
        device_id: foreign(Device, 2),
        name: "Strych",
        type: :motion_sensor,
        number: 16,
        mode: :input
      )
    ]
  end

  def actions do
    [
      Action.new(
        name: "Cały dom",
        active: true,
        function: "CloseSunblind",
        arguments: [
          up: foreign(Port, [11, 12, 13, 14, 15, 16]),
          down: foreign(Port, [11, 14, 15, 16])
        ],
        frequency: 15000
      ),
      Action.new(
        name: "Salon schody",
        active: false,
        function: "AutoLights",
        frequency: 15000,
        port_id: 18,
        params: %{on_timeout: 30000}
      ),
      Action.new(
        name: "Salon stół",
        active: false,
        function: "AutoLights",
        frequency: 15000,
        port_id: foreign(Port, 41),
        params: %{on_timeout: 30000}
      ),
      Action.new(
        name: "Salon wypoczynek",
        active: false,
        function: "AutoLights",
        frequency: 15000,
        port_id: foreign(Port, 42),
        params: %{on_timeout: 30000}
      ),
      Action.new(
        name: "Pokój Przemek",
        active: true,
        function: "CloseSunblind",
        arguments: [up: [], down: foreign(Port, [12, 13])],
        frequency: 15000
      ),
      Action.new(
        name: "Sypialnia światło",
        active: false,
        function: "AutoLights",
        frequency: 15000,
        port_id: foreign(Port, 38),
        params: %{on_timeout: 30000}
      ),
      Action.new(
        name: "Światło poddasze",
        active: true,
        arguments: [up_down: {:foreign, DB.Port, 25}],
        function: "AutoLights",
        frequency: 15000,
        port_id: foreign(Port, 48),
        params: %{on_timeout: 30_000}
      ),
      Action.new(
        name: "Wiatrak łazienka",
        active: false,
        function: "AutoLights",
        frequency: 1000,
        port_id: foreign(Port, 47),
        params: %{on_timeout: 10000}
      ),
      Action.new(
        name: "Grupa świateł salon",
        arguments: [
          up_down: [
            {:foreign, DB.Port, 5},
            {:foreign, DB.Port, 6},
            {:foreign, DB.Port, 8},
            {:foreign, DB.Port, 9}
          ]
        ],
        active: true,
        function: "ToggleGroup",
        frequency: 1000
      ),
      Action.new(
        name: "Sterowanie ściemniaczem TV",
        active: true,
        function: "DimmerController"
      ),
      Action.new(
        name: "Sterowanie ściemniaczem Kanapa",
        active: true,
        function: "DimmerController"
      ),
      Action.new(
        name: "Odczyt wejść satel",
        active: true,
        function: "ReadInputs",
        arguments: [up_down: foreign(Device, 2)]
      ),
      Action.new(
        name: "Odczyt temperatury mega",
        active: true,
        function: "ReadTemperature",
        arguments: [up_down: foreign(Device, 1)]
      ),
      Action.new(
        name: "Heartbeat mega",
        active: true,
        function: "Heartbeat",
        arguments: [up_down: foreign(Device, 1)]
      )
    ]
  end

  def jobs do
    [
      ScheduleJob.new(
        expr: "*/2 * * * * *",
        extended: true,
        action_id: foreign(Action, 12)
      ),
      ScheduleJob.new(
        expr: "* 17 * * * *",
        state: :up,
        action_id: foreign(Action, 1)
      ),
      ScheduleJob.new(
        expr: "* 8 * * * *",
        action_id: foreign(Action, 1)
      ),
      ScheduleJob.new(
        expr: "* 10 * * * *",
        state: :down,
        action_id: foreign(Action, 5)
      ),
      ScheduleJob.new(
        expr: "*/1 * * * * *",
        action_id: foreign(Action, 14)
      ),
      ScheduleJob.new(
        expr: "*/30 * * * * *",
        action_id: foreign(Action, 13)
      )
    ]
  end

  def meters do
    [Meter.new(name: "Centrala skrzynka", address: "(ÿñ%!", type: :temperature)]
  end

  def rf_buttons do
    [
      RfButton.new(
        name: "8365000A2C-1",
        key_value: "1198E8",
        on_click_action: [btn_pga(1, Port, 2), btn_pga(2, Port, 18)]
      ),
      RfButton.new(
        name: "8365000A2C-2",
        key_value: "1198EC",
        on_click_action: [btn_pga(1, Port, 3), btn_pga(2, Port, 19)]
      ),
      RfButton.new(
        name: "8365000A2C-3",
        key_value: "1198E4",
        on_click_action: [btn_pga(1, Port, 8), btn_pga(2, Action, 10)]
      ),
      RfButton.new(
        name: "8365000A2C-4",
        key_value: "1198E9",
        on_click_action: [btn_pga(1, Port, 9), btn_pga(2, Action, 11)]
      ),
      RfButton.new(
        name: "8365000A2C-5",
        key_value: "1198E2",
        on_click_action: [btn_pga(1, Port, 5)]
      ),
      RfButton.new(
        name: "8365000A2C-6",
        key_value: "1198E5",
        on_click_action: [btn_pga(1, Port, 6)]
      ),
      RfButton.new(
        name: "8365000A2C-7",
        key_value: "1198E1",
        on_click_action: [btn_pga(1, Action, 9)]
      ),
      RfButton.new(name: "8365000A2C-8", key_value: "1198E3", mode: :page),
      RfButton.new(
        name: "836500138C-1",
        key_value: "6195B8",
        on_click_action: [btn_pga(1, Port, 23)]
      ),
      RfButton.new(name: "836500138C-2", key_value: "6195BC"),
      RfButton.new(name: "836500138C-3", key_value: "6195B4"),
      RfButton.new(name: "836500138C-4", key_value: "6195B9"),
      RfButton.new(name: "836500138C-5", key_value: "6195B2"),
      RfButton.new(name: "836500138C-6", key_value: "6195B5"),
      RfButton.new(name: "836500138C-7", key_value: "6195B1"),
      RfButton.new(name: "836500138C-8", key_value: "6195B3"),
      RfButton.new(
        name: "8365001354-1",
        key_value: "9B6758",
        on_click_action: [btn_pga(1, Port, 25)]
      ),
      RfButton.new(
        name: "8365001354-2",
        key_value: "9B675C",
        on_click_action: [btn_pga(1, Port, 26)]
      ),
      RfButton.new(
        name: "8365001354-3",
        key_value: "9B6754",
        on_click_action: [btn_pga(1, Port, 27)]
      ),
      RfButton.new(
        name: "8365001354-4",
        key_value: "9B6759",
        on_click_action: [btn_pga(1, Port, 28)]
      ),
      RfButton.new(name: "8365001354-5", key_value: "9B6752"),
      RfButton.new(name: "8365001354-6", key_value: "9B6755"),
      RfButton.new(name: "8365001354-7", key_value: "9B6751"),
      RfButton.new(name: "8365001354-8", key_value: "9B6753")
    ]
  end

  def foreign(mod, ids) when is_list(ids) do
    Enum.map(ids, fn i -> foreign(mod, i) end)
  end

  def foreign(mod, id) do
    {:foreign, mod, id}
  end

  def btn_pga(page, mod, id) do
    {page, foreign(mod, id)}
  end

  def pages do
    [
      Page.new(
        name: "Piwnica",
        order: 1,
        content: foreign(Port, [33, 34, 35, 36]) ++ foreign(Action, [])
      ),
      Page.new(
        name: "Parter",
        order: 2,
        content: foreign(Port, [10, 23, 37, 38, 39]) ++ foreign(Action, [6])
      ),
      Page.new(
        name: "Salon",
        order: 3,
        content:
          foreign(Port, [1, 4, 7, 18, 19, 15, 16, 40, 41, 42, 43, 44]) ++ foreign(Action, [2, 3, 4, 9, 10, 11])
      ),
      Page.new(
        name: "Piętro",
        order: 4,
        content: foreign(Port, [12, 13, 14, 24, 45, 46, 47]) ++ foreign(Action, [5, 8])
      ),
      Page.new(
        name: "Poddasze",
        order: 5,
        content: foreign(Port, [20, 25, 26, 27, 28, 29, 30, 48]) ++ foreign(Action, [7])
      )
    ]
  end

  def houses do
  end

  def test_data_for_orginal_arduino() do
    dev = Device.new(ip: "192.168.2.107", name: "OriginalArduinoTest", type: "OriginalArduino") |> Device.insert()
      Port.new(
        device_id: foreign(Device, dev.id),
        name: "Test light1",
        type: :light,
        number: 18,
        mode: :output,
        inverted_logic: true
      )
      |> Port.insert()
  end

end
