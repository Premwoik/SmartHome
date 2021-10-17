defmodule DB.Stats.DeviceJournal do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Query

  alias DB.Stats.DeviceJournal
  import DB.Stats.Data, only: [set_from_t: 2, set_to_t: 2]
  @repo DB.StatsRepo

  @derive {Poison.Encoder, except: [:__meta__]}
  schema "device_journals" do
    field(:device_id, :integer)
    field(:name, :string)
    field(:type, :string)
    field(:info, :string)
    field(:arguments, :string)
    field(:result, :string)
    timestamps()
  end

  @spec log_use(map(), atom(), any) :: any()
  def log_use(device_id, res, name, args \\ nil) do
    name = to_string(name)

    case res.result do
      [{_, {:ok, res}} | _] ->
        add(device_id, "NORMAL", name, "", args, res)

      [{_, :ok} | _] ->
        add(device_id, "NORMAL", name, "", args, nil)

      [{_, {:error, res}} | _] ->
        add(device_id, "ERROR", name, "", args, res)

      [{_, :timeout} | _] ->
        add(device_id, "TIMEOUT", name, "", args, nil)

      res ->
        add(device_id, "UNKNOWN", name, "", args, res)
    end

    :ok
  end

  def add(device_id, type, name, info, args, result) do
    e_result = encode(result)
    e_args = encode(args)

    %DeviceJournal{
      type: type,
      name: name,
      info: info,
      arguments: e_args,
      result: e_result,
      device_id: device_id
    }
    |> DB.StatsRepo.insert!()
  end

  defp encode(data) do
    case Poison.encode(data) do
      {:ok, res} -> res
      _ -> "#{inspect(data, binaries: :as_lists)}"
    end
  end

  def get(device, limit \\ 1000, from \\ nil)

  def get(device_id, limit, from) do
    DeviceJournal
    |> where([d], d.device_id == ^device_id)
    |> set_from_t(from)
    |> order_by([d], desc: d.inserted_at)
    |> limit(^limit)
    |> @repo.all()
  end

  def delete_older(date) do
    from(d in DeviceJournal, where: d.inserted_at <= ^date)
    |> @repo.delete_all()
  end

  def collect(names, from, to) do
    DeviceJournal
    |> where([d], d.type == "NORMAL")
    |> set_names(names)
    |> set_from_t(from)
    |> set_to_t(to)
    |> @repo.all()
  end

  def set_names(query, nil), do: query

  def set_names(query, names) when is_list(names) do
    Enum.reduce(names, query, fn name, acc -> where(acc, [d], d.name == ^name) end)
  end

  def set_names(query, name), do: where(query, [d], d.name == ^name)
end
