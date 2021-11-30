defmodule Core.Device.BasementPi do
  @moduledoc """

    API:
  - run_circut/1
  - get_config/0
  - update_config/1
  """

  use GenServer

  require Record

  alias DB.Data.Device
  alias DB.Data.Port
  alias DB.Proc.PortListProc
  alias Core.Device.Static.Response

  @behaviour Core.Device
  @behaviour Core.Device.BasicIO

  @type circut_name() :: :high | :low
  @type circut_status() :: :idle | :running | :blocked
  @type circut_planned_run() ::
          {day :: :null | atom(), start_time :: :calendar.time(), duration :: :calendar.time()}

  Record.defrecord(:circut,
    name: :undefined,
    break_duration: {0, 0, 0},
    running_duration: {0, 0, 0},
    stop_timer_ref: :null,
    max_temp: 0.0,
    min_temp: 0.0,
    status: :idle,
    valve_pin: 0,
    thermometer_id: '',
    auto_allow: false,
    planned_runs: [],
    current_temp: :null
  )

  @type circut() ::
          Record.record(
            :circut,
            name :: atom(),
            break_duration :: :calendar.time(),
            running_duration :: :calendar.time(),
            stop_timer_ref :: :null | reference(),
            max_temp :: float(),
            min_temp :: float(),
            status :: circut_status(),
            valve_pin :: integer(),
            thermometer_id :: charlist(),
            auto_allow :: boolean(),
            planned_run :: [circut_planned_run()],
            current_temp :: :null | float()
          )

  Record.defrecord(:state,
    circuts: [],
    temp_read_interval: {0, 0, 0},
    pomp_pin: 0,
    boiler_thermometer_id: '',
    boiler_min_temp: 0.0,
    boiler_temp: :null
  )

  @type config() ::
          Record.record(
            :state,
            circuts :: [circut()],
            temp_read_interval :: :calendar.time(),
            pomp_pin :: integer(),
            boiler_thermometer_id :: charlist(),
            boiler_min_temp :: float(),
            boiler_temp :: float() | :null
          )

  #############################################################################
  ### API Beh
  #############################################################################

  def run_circut(device, name) do
    node = String.to_atom(device.ip)

    try do
      :rpc.call(node, :heating_server, :run_circut, [name])
    rescue
      err ->
        IO.inspect(err)
        {:error, :node_issue}
    end
  end

  @spec get_temps(Device.t()) :: {:ok, list()} | {:error, term()}
  def get_temps(%{id: node}) do
    node = String.to_atom(node)

    try do
      :rpc.call(node, :heating_server, :get_temps, [])
    rescue
      err ->
        IO.inspect(err)
        {:error, :node_issue}
    end
  end

  @spec get_config(Device.t()) :: config()
  def get_config(%{ip: node}) do
    try do
      node = String.to_atom(node)
      {:ok, conf} = :rpc.call(node, :heating_server, :get_config, [])
      state2 = state(conf) |> Enum.into(%{})

      state3 = %{
        state2
        | circuts: Enum.map(state2.circuts, fn c -> Enum.into(circut(c), %{}) end)
      }

      {:ok, state3}
    rescue
      err ->
        IO.inspect(err)
        {:error, :node_issue}
    end
  end

  #############################################################################
  ### Device beh
  #############################################################################

  @impl true
  def need_process?() do
    true
  end

  @impl true
  def start_link(ip, _port, opts) do
    map =
      [{:node, List.to_atom(ip)} | opts]
      |> Map.new()

    GenServer.start_link(__MODULE__, map)
  end

  #############################################################################
  ### BasicIO beh
  #############################################################################

  @impl true
  def set_outputs(device, [%Port{number: n, state: %{"value" => true}} = port]) do
    name =
      case n do
        0 -> :low
        _ -> :high
      end

    case run_circut(device, name) do
      :ok ->
        %Response{ok: [port], result: [{device.id, :ok}], save: false}

      err ->
        Response.error({device.id, err}, [port])
    end
  end

  def set_outputs(device, ports) do
    Response.error({device.id, :wrong_args}, ports)
  end

  @impl true
  def read_outputs(device) do
    Response.error({device.id, :not_implemented}, [])
    {:error, :not_implemented}
  end

  @impl true
  def read_inputs(device) do
    Response.error({device.id, :not_implemented}, [])
  end

  @impl true
  def heartbeat(device) do
    Response.error({device.id, :not_implemented}, [])
  end

  #############################################################################
  ### GenServer 
  #############################################################################

  @impl GenServer
  def init(%{device_id: device_id, node: node} = opts) do
    {:ok, [%{id: id0}, %{id: id1}]} = PortListProc.identify(device_id, [0, 1])
    pid = self()

    try do
      :rpc.call(node, :heating_server, :register_observer, [pid])
    rescue
      err ->
        IO.inspect(err)
    end

    {:ok, Map.put(opts, :port_ids, [id0, id1])}
  end

  @impl GenServer
  def handle_info({:status_update, data}, %{port_ids: [id0, id1]} = state) do
    with {:ok, s0} <- Keyword.fetch(data, :low),
         {:ok, s1} <- Keyword.fetch(data, :high) do
      v0 = s0 == :running
      v1 = s1 == :running
      PortListProc.update_state(id0, %{"status" => to_string(s0), "value" => v0})
      |> broadcast()
      PortListProc.update_state(id1, %{"status" => to_string(s1), "value" => v1})
      |> broadcast()
    end

    {:noreply, state}
  end

  def handle_info({:temp_update, data}, %{port_ids: [id0, id1]} = state) do
    with {:ok, s0} <- Keyword.fetch(data, :low),
         {:ok, s1} <- Keyword.fetch(data, :high) do
      PortListProc.update_state(id0, %{"temp" => s0})
      |> broadcast()
      PortListProc.update_state(id1, %{"temp" => s1})
      |> broadcast()
    end

    {:noreply, state}
  end

  #############################################################################
  ### Internals
  #############################################################################
  def broadcast({:ok, port}) do
    Core.Broadcast.broadcast_item_change(:circut, port)
  end
  def broadcast(_) do
    :ok
  end
end
