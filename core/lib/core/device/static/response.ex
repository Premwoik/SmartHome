defmodule Core.Device.Static.Response do
  @moduledoc false

  require Logger
  alias Core.Device.Static.Response
  import TypeClass
  use Witchcraft.Semigroup

  @type t() :: %Response{ok: list(any), error: list(any), result: list(any)}

  defstruct ok: [], error: [], result: []

  defimpl TypeClass.Property.Generator, for: Response do
    def generate(_), do: %Response{} |> Map.from_struct()
  end

  definst Witchcraft.Semigroup, for: Response do
    def append(a, b) do
      appended = Enum.map(Map.from_struct(a), fn {k, v} -> {k, v <> Map.get(b, k)} end)
      struct(%Response{}, appended)
    end
  end

  definst Witchcraft.Functor, for: Response do
    def map(%{ok: []} = resp, _) do
      resp
    end

    def map(%{ok: ok} = resp, fun) do
      Map.put(resp, :ok, fun.(ok))
    end
  end

  def result(%{result: r}, id) do
    Enum.find(r, &(elem(&1, 0) == id)) |> elem(1)
  end

  def wrap_same({:ok, r} = res, device) do
    wrap(res, device, r)
  end

  def wrap_same(res, device), do: wrap(res, device)

  def wrap_with_ports({:ok, numbers} = res, device) do
    ports = DB.Port.identify(device.id, numbers)
    wrap(res, device, ports)
  end

  def wrap_with_ports(res, device), do: wrap(res, device)

  def wrap(result, device, ports \\ []) do
    case result do
      :ok ->
        %Response{ok: ports, result: [{device.id, result}]}

      {:ok, _} ->
        %Response{ok: ports, result: [{device.id, result}]}

      {:error, _} ->
        %Response{error: ports, result: [{device.id, result}]}

      e ->
        Logger.error("Unsupported result type: #{inspect(e)}")
        %Response{error: ports, result: [{device.id, result}]}
    end
  end

  def error(error, items \\ []) do
    %Response{result: [error], error: items}
  end
end
