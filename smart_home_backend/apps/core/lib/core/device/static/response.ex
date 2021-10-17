defmodule Core.Device.Static.Response do
  @moduledoc false

  require Logger

  alias Core.Device.Static.Response
  alias DB.Proc.PortListProc

  @type t() :: %Response{ok: list(any), error: list(any), result: list(any)}

  defstruct ok: [], error: [], result: []

  @spec fold([Response.t()]) :: Response.t()
  def fold([]), do: %Response{}

  def fold(list) do
    Enum.reduce(list, %{}, fn r, acc -> Map.merge(r, acc, fn _, v1, v2 -> v1 ++ v2 end) end)
  end

  @spec map(Response.t(), fun()) :: Response.t()
  def map(resp, fun) do
    case resp do
      %{ok: []} = resp -> resp
      %{ok: ok} = resp -> Map.put(resp, :ok, fun.(ok))
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
    {:ok, ports} = PortListProc.identify(device.id, numbers)
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

  def error(error, items \\ [])

  def error(error, items) when is_list(items) do
    %Response{result: [error], error: items}
  end

  def error(error, item) do
    %Response{result: [error], error: [item]}
  end

  def ok(items) when is_list(items) do
    %Response{result: [:ok], ok: items}
  end

  def ok(item) do
    %Response{result: [:ok], ok: [item]}
  end
end