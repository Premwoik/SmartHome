defmodule Core.Device.BasementPi do
  @moduledoc """

    API:
  - run_circut/1
  - get_config/0
  - update_config/1
  """

  use GenServer

  require Logger

  alias DB.Data.Device
  alias DB.Data.Port
  alias DB.Proc.PortListProc
  alias Core.Device.Static.Response
  alias Core.Device.BasicIO

  @behaviour Core.Device
  @behaviour Core.Device.BasicIO

  @type circut_name() :: :high | :low
  @type circut_status() :: :idle | :running | :blocked
  @type circut_planned_run() ::
          {day :: :null | atom(), start_time :: :calendar.time(), duration :: :calendar.time()}

  @type config :: :heating_api.config()

  @observer_check_time 60_000

  ## Api behaviour

  @spec run_circut(Device.t(), atom() | integer()) :: :ok | :node_issue
  def run_circut(device, id), do: :heating_api.run_circut(device, id)

  @spec get_temps(Device.t()) :: :ok | :node_issue
  def get_temps(device), do: :heating_api.get_temps(device)

  @spec get_config(Device.t()) :: {:ok, config()} | :node_issue
  def get_config(device), do: :heating_api.get_config(device)

  def get_local_config(device),
    do: GenServer.call(String.to_existing_atom(device.name), :get_config)

  @spec set_config(Device.t(), config()) :: :ok
  def set_config(device, config),
    do: GenServer.cast(String.to_existing_atom(device.name), {:set_config, config})

  ## Device behaviour

  @impl Core.Device
  def need_process?(), do: true

  @impl Core.Device
  def start_link(ip, _port, opts) do
    map =
      [{:node, List.to_atom(ip)} | opts]
      |> Map.new()

    GenServer.start_link(__MODULE__, map, name: String.to_existing_atom(map.device.name))
  end

  ## BasicIO behaviour

  @impl BasicIO
  def set_outputs(device, [%Port{number: number, type: :circut, state: %{"value" => true}} = port]) do
    case run_circut(device, number) do
      :ok ->
        %Response{ok: [port], result: [{device.id, :ok}], save: false}

      err ->
        Response.error({device.id, err}, [port])
    end
  end

  @impl BasicIO
  def set_outputs(device, [%Port{number: n, state: %{"value" => state}} = port]) do
    case :heating_api.write_pin(device, n, state) do
      :ok ->
        %Response{ok: [port], result: [{device.id, :ok}], save: true}

      err ->
        Response.error({device.id, err}, [port])
    end
  end

  def set_outputs(device, ports) do
    Response.error({device.id, :wrong_args}, ports)
  end

  @impl BasicIO
  def read_outputs(device) do
    Response.error({device.id, :not_implemented}, [])
  end

  @impl BasicIO
  def read_inputs(device) do
    Response.error({device.id, :not_implemented}, [])
  end

  @impl BasicIO
  def heartbeat(device) do
    Response.error({device.id, :not_implemented}, [])
  end

  ## GenServer behaviour

  @impl GenServer
  def init(%{device: device} = opts) do
    config = do_init(device, self())
    {:ok, Map.put(opts, :config, config)}
  end

  @impl GenServer
  def terminate(_reason, %{device: device}) do
    :heating_api.unregister_observer(device, self())
    :ok
  end

  @impl GenServer
  def handle_call(:get_config, _From, state) do
    {:reply, state.config, state}
  end

  @impl GenServer
  def handle_cast({:set_config, config}, state) do
    {:noreply, Map.put(state, :config, config)}
  end

  @impl GenServer
  def handle_info({:status_update, data}, %{config: %{circuts: circuts}} = state) do
    Enum.zip(data, circuts)
    |> Enum.each(fn {{name, state}, {name, id}} ->
      value = state == :running

      PortListProc.fast_update_state(id, %{"status" => to_string(state), "value" => value})
      |> broadcast()
    end)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:temp_update, data}, %{config: %{circuts: circuts}} = state) do
    Enum.zip(data, circuts)
    |> Enum.each(fn {{name, temp}, %{name: name, port_id: id}} ->
      PortListProc.fast_update_state(id, %{"temp" => temp})
      |> broadcast()
    end)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(:init, %{device: device} = state) do
    config = do_init(device, self())
    {:noreply, Map.put(state, :config, config)}
  end

  @impl GenServer
  def handle_info(:check_subscription, %{device: device} = state) do
    do_check_subscription(device, self())
    {:noreply, state}
  end

  ## Internal

  def do_check_subscription(device, pid) do
    Process.send_after(pid, :check_subscription, @observer_check_time)

    unless :heating_api.is_registered_observer(device, pid) do
      :heating_api.register_observer(device, pid)
    else
      :ok
    end
  end

  @spec do_init(Device.t(), pid()) :: config() | nil
  def do_init(device, pid) do
    with :ok <- do_check_subscription(device, pid),
         {:ok, config} <- :heating_api.get_config(device) do
      Logger.info("Heating client `#{to_string(device.name)}` initialized successfully!")
      add_port_ids(device, config)
    else
      _ ->
        Logger.warn(
          "Heating client `#{to_string(device.name)}` initialization failed. Will try again after 5sec!"
        )

        Process.send_after(pid, :init, 5_000)
        nil
    end
  end

  @spec add_port_ids(Device.t(), config()) :: config()
  defp add_port_ids(device, %{circuts: circuts} = config) do
    nums = Enum.with_index(circuts) |> Enum.map(&elem(&1, 1))

    circuts =
      PortListProc.identify!(device.id, nums)
      |> Enum.zip(circuts)
      |> Enum.map(fn {%{id: id}, circut} -> Map.put(circut, :port_id, id) end)

    %{config | circuts: circuts}
  end

  defp broadcast({:ok, port}) do
    Core.Broadcast.broadcast_item_change(:circut, port)
  end

  defp broadcast(_) do
    :ok
  end
end
