defmodule Device.Controller do
  @moduledoc false

  alias DB.Dao
  alias Device.Command
  require Logger

## Api
  @doc """
    set_outputs_id
  """
  @spec set_outputs_id(atom, list(integer), boolean) :: atom
  def set_outputs_id(deviceAtom, ids, state) do
    ports = Dao.get_ports ids
    set_outputs(deviceAtom, ports, state)
  end

  @doc """
      set_output
  """
  @spec set_outputs(atom, list(map), boolean) :: atom
  def set_outputs(deviceAtom, ports, state) do
    #TODO handle situation when device is not connected or dont response
    portIds = Enum.map ports, &(&1.id)
    {timeoutPorts, ports_} = Enum.split_with ports, &(&1.timeout > 0)
    true = set_ports deviceAtom, ports_, state
    true = set_timeout_ports deviceAtom, timeoutPorts
    Dao.update_ports_state(portIds, state)
    :ok
  end


  @doc """
    set_dim_fill
  """
  @spec set_dim_fill(integer(), integer()) :: atom()
  def set_dim_fill(id, fill) do
    {port, dimmer} = Dao.get_dimmer(id) |> List.first
    time = fill_to_time dimmer, fill
    if time > 0 do
      device = Dao.get_device_atom(port.device_id)
      set_outputs  device, [port], true
      direction = dimmer.direction * -1
      Task.start_link fn -> turn_off_dimmers device, [port], {time, fill, direction} end
    end
    :ok
  end


  @doc """
    set_dim_lights_id
  """
  @spec set_dim_lights_id(list(integer), boolean) :: atom
  def set_dim_lights_id(ids, state) do
    lights = Dao.get_ports(ids)
    set_dim_lights(lights, state)
  end

  @doc """
    set_dim_lights
  """
  @spec set_dim_lights(list(map), boolean()) :: atom()
  def set_dim_lights(lights, state)
  def set_dim_lights(lights, false) do
    #    turn off lights
    (Enum.group_by lights, &(&1.device_id))
    |> (Enum.each fn {k, v} -> set_outputs (Dao.get_device_atom k), v, false end)

    #    reset power cut dimmers
    (Enum.map lights, &(&1.id))
    |> Dao.get_down_dimmers
    |> (Dao.update_dimmers_fill 0, 1)
  end
  def set_dim_lights(lights, true) do
    #    turn on lights
#    zwraca sciemniacze podzielone na urzadzenia
    dimmMap = Dao.match_lights_to_dimmers (Enum.map lights, &(&1.id))
    (Enum.group_by lights, &(&1.device_id))
    |> (Enum.each fn {dId, ls} -> dimmer_device dId, ls, dimmMap[dId] end)
  end


  #  Privates

  @spec turn_off_dimmers(atom(), list(map()), tuple()) :: atom()
  defp turn_off_dimmers(deviceAtom, dimPorts, {time, fill, dir}) do
#    Logger.info("dimmer_off_task") #TODO change to debug or remove
    receive do
      _ -> Logger.error("Dimmer_off_task shouldn't receive message")
    after
      time -> set_outputs  deviceAtom, dimPorts, false
              Dao.update_dimmers_fill (Enum.map dimPorts, &(&1.id)), fill, dir
    end
    :ok
  end

  defp dimmer_device(deviceId, lights, nil) do
    (Dao.get_device_atom deviceId)
    |> (set_outputs lights, true)
  end

  defp dimmer_device(deviceId, lights, dimPorts) do
    deviceAtom = Dao.get_device_atom deviceId
    set_outputs deviceAtom, lights ++ dimPorts, true
#    TODO change this hardcoded values?
    Task.start_link fn -> turn_off_dimmers deviceAtom, dimPorts, {4_500, 100, -1} end
  end

  @spec set_ports(atom(), list(map()), boolean()) :: boolean()
  defp set_ports(_, [], _), do: true
  defp set_ports(deviceAtom, ports, state) do
    portNums = Enum.map ports, &(&1.number)
    Command.set_output_state deviceAtom, portNums, state
  end

  @spec set_timeout_ports(atom(), list(map())) :: boolean()
  defp set_timeout_ports(deviceAtom, ports) do
    true = set_ports(deviceAtom, ports, true)
    (Enum.group_by ports, &(&1.timeout))
    |> (Enum.each fn {k, v} -> Task.start fn -> handle_timeout deviceAtom, v, k end end)
    true
  end

  @spec handle_timeout(atom(), list(map()), integer()) :: any()
  defp handle_timeout(deviceAtom, ports, timeout) do
    receive do
    after
      timeout -> true = set_ports(deviceAtom, ports, false)
    end
  end

  @spec fill_to_time(map, integer) :: integer
  def fill_to_time(dimmer, newFill) do
    res = (newFill - dimmer.fill) * dimmer.direction
    IO.inspect res
    cond do
      res > 0 ->
        get_time(dimmer.time, res)
      res == 0 ->
        0
      dimmer.direction > 0 ->
        get_time(dimmer.time, 200 - dimmer.fill - newFill)
      true ->
        get_time(dimmer.time, dimmer.fill + newFill)
    end
  end

  @spec get_time(integer, integer) :: integer
  defp get_time(max_time, fill) do
    cond do
      fill > 150 -> max_time * 1.75
      fill > 125 -> max_time * 1.50
      fill > 100 -> max_time * 1.25
      fill > 75 -> max_time
      fill > 50 -> max_time * 0.75
      fill > 25 -> max_time * 0.5
      fill > 0 -> max_time * 0.25
      true -> 0
    end
    |> round
  end
end
