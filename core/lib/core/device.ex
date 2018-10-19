defmodule Core.Device do
  @moduledoc false

  @callback set_outputs(device :: %DB.Device{}, ids :: list(integer), state :: boolean) :: any
  @callback read_outputs(device :: %DB.Device{}) :: list({integer, boolean})
  @callback read_active_outputs(device :: map) :: list(integer)

  alias DB.Port

  def read_active_outputs_helper(device) do
    module(device).read_active_outputs(device)
  end

  def set_outputs_helper(ports, state) do
    ports
    |> Enum.group_by(&(&1.device))
    |> Enum.map(
         fn {d, p} ->
           case module(d).set_outputs d, ports_to_num(p), state do
             :ok -> :ok
             {:error, err} -> {:error, p, err}
           end
         end
       )
    |> Enum.filter(fn x -> x != :ok end)
    |> case do
         [] -> :ok
         errors ->
           errors
           |> Enum.reduce(
                {:error, [], []},
                fn {:error, p1, err}, {:error, p2, err_list} ->
                  {:error, p1 ++ p2, [err | err_list]}
                end
              )
       end
  end

  # Privates

  defp ports_to_num(ports) do
    Enum.map(ports, &(&1.number))
  end

  defp module(device) do
    String.to_existing_atom("Elixir." <> device.type)
  end



end
