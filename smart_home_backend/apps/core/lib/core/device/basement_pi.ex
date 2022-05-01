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

  # def set_config(device, config), do: :heating_api.set_config(device, config)

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
  def set_outputs(device, [%Port{number: number, state: %{"value" => true}} = port]) do
    case run_circut(device, number) do
      :ok ->
        %Response{ok: [port], result: [{device.id, :ok}], save: false}

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
    {ids, config} = try_init(device, self())
    opts = Map.put(opts, :config, config)

    {:ok, Map.put(opts, :port_ids, ids)}
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
  def handle_info({:status_update, data}, %{port_ids: ids} = state) do
    Enum.zip(data, ids)
    |> Enum.each(fn {{name, state}, {name, id}} ->
      value = state == :running

      PortListProc.update_state(id, %{"status" => to_string(state), "value" => value})
      |> broadcast()
    end)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:temp_update, data}, %{port_ids: ids} = state) do
    Enum.zip(data, ids)
    |> Enum.each(fn {{name, temp}, {name, id}} ->
      PortListProc.update_state(id, %{"temp" => temp})
      |> broadcast()
    end)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(:try_init, %{device: device} = state) do
    {ids, config} = try_init(device, self())
    state = Map.put(state, :config, config)

    {:noreply, Map.put(state, :port_ids, ids)}
  end

  ## Internal

  def try_init(device, pid) do
    with :ok <- :heating_api.register_observer(device, pid),
         {:ok, config} <- :heating_api.get_config(device) do
      Logger.info("Heating client `#{to_string(device.name)}` initialized successfully!")
      {get_port_ids(device, config), config}
    else
      _ ->
        Logger.warn(
          "Heating client `#{to_string(device.name)}` initialization failed. Will try again after 5sec!"
        )

        Process.send_after(pid, :try_init, 5_000)
        {[], %{}}
    end
  end

  @spec get_port_ids(Device.t(), config()) :: [{atom(), integer()}]
  defp get_port_ids(device, %{circuts: circuts}) do
    nums = Enum.with_index(circuts) |> Enum.map(&elem(&1, 1))

    pairs =
      PortListProc.identify!(device.id, nums)
      |> Enum.zip(circuts)
      |> Enum.map(fn {%{id: id}, %{name: name}} -> {name, id} end)

    pairs
  end

  defp broadcast({:ok, port}) do
    Core.Broadcast.broadcast_item_change(:circut, port)
  end

  defp broadcast(_) do
    :ok
  end
end
