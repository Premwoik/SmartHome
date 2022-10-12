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

  @type circut_name() :: atom()
  @type port_id() :: integer()

  @type config :: :heating_api.config()

  @type state_t() :: %{
          atom() => term(),
          device: Device.t(),
          registered: boolean(),
          circuts: %{circut_name() => port_id()}
        }

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

  @spec set_config(Device.t(), config()) :: :ok | :node_issue
  def set_config(device, config),
    do: GenServer.call(String.to_existing_atom(device.name), {:set_config, config})

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
  def init(%{device: device}) do
    state = %{device: device, registered: false, circuts: %{}}
    {:ok, do_init(state, self())}
  end

  @impl GenServer
  def terminate(_reason, %{device: device}) do
    :heating_api.unregister_observer(device, self())
    :ok
  end

  @impl GenServer
  def handle_call(:get_config, _From, state) do
    {:reply, state, state}
  end

  def handle_call({:set_config, config}, _From, state) do
    new_state = Map.merge(state, config)
    result = :heating_api.set_config(new_state.device, new_state)
    {:reply, result, new_state}
  end

  @impl GenServer
  def handle_info({:status_update, data}, %{circuts: circuts} = state) do
    Enum.each(data, fn {name, state} ->
      case circuts[name] do
        nil ->
          Logger.warn("Cannot find `port_id` for circut: #{to_string(name)}")

        %{v_port_id: port_id} ->
          value = state == :running
          Logger.debug("Read thermometer port_id=#{port_id} status=#{state}")

          PortListProc.fast_update_state(port_id, %{
            "status" => to_string(state),
            "value" => value
          })
          |> broadcast()
      end
    end)

    {:noreply, state}
  end

  def handle_info({:temp_update, data}, %{circuts: circuts} = state) do
    Enum.each(data, fn {name, temp} ->
      case circuts[name] do
        nil ->
          Logger.warn("Cannot find `port_id` for circut: #{to_string(name)}")

        %{v_port_id: port_id} ->
          Logger.debug("Read thermometer port_id=#{port_id} temp=#{temp}")

          PortListProc.fast_update_state(port_id, %{"temp" => temp})
          |> broadcast()
      end
    end)

    {:noreply, state}
  end

  def handle_info(:init, state) do
    {:noreply, do_init(state, self())}
  end

  def handle_info(:check_subscription, %{device: device} = state) do
    do_check_subscription(device, self())
    {:noreply, state}
  end

  ## Internal

  def do_check_subscription(device, pid) do
    Process.send_after(pid, :check_subscription, @observer_check_time)

    unless :heating_api.is_registered_observer(device, pid) do
      Logger.info("Subscribing to the #{device.name} heating_server events")
      :heating_api.register_observer(device, pid)
    end
  end

  @spec do_init(state_t(), pid()) :: state_t()
  def do_init(%{device: device} = state, pid) do
    with :ok <- do_check_subscription(device, pid),
         {:ok, config} <- :heating_api.get_config(device) do
      Logger.info("Heating client `#{to_string(device.name)}` initialized successfully!")
      update_state(%{state | registered: true}, config)
    else
      _ ->
        Logger.warn(
          "Heating client `#{to_string(device.name)}` initialization failed. Will try again after 5sec!"
        )

        Process.send_after(pid, :init, 5_000)
        state
    end
  end

  @spec update_state(state_t(), config()) :: state_t()
  defp update_state(state = %{device: device}, config = %{circuts: circuts}) do
    nums = Enum.to_list(1..length(circuts))

    circuts =
      PortListProc.identify!(device.id, nums)
      |> Enum.sort_by(& &1.number)
      |> Enum.zip(circuts)
      |> Enum.map(fn {%{id: id, name: v_name, number: num}, %{name: name} = circut} ->
        update_circut_port(id, circut)
        # Add some virtual fields with information about smart_home port
        {name, Map.merge(circut, %{v_id: num, v_port_id: id, v_name: v_name})}
      end)
      |> Map.new()

    Map.merge(config, %{state | circuts: circuts})
  end

  def update_circut_port(port_id, %{current_temp: current_temp, status: status}) do
    value = status == :running

    PortListProc.fast_update_state(port_id, %{
      "value" => value,
      "temp" => current_temp,
      "status" => to_string(status)
    })
  end

  defp broadcast({:ok, port}) do
    Core.Broadcast.broadcast_item_change(:circut, port)
  end

  defp broadcast(_) do
    :ok
  end
end
