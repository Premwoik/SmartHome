defmodule DB.Proc.Beh do
  @type resp(i) :: {:ok, i} | {:error, Ecto.Changeset.t()}
  @type resp!(i) :: i | nil
  @type item_id() :: integer()

  @typep resp() :: resp(item())
  @typep resp!() :: resp!(item())
  @typep item() :: struct() | map()

  @callback start_link(any()) :: GenServer.on_start()
  @callback force_read_all() :: :ok

  @callback get(item_id()) :: resp()
  @callback get!(item_id()) :: resp!()

  @callback list_all() :: {:ok, [item()]}
  @callback list_all!() :: [item()]

  @callback update!(item_id(), item()) :: resp!()
  @callback update(item_id(), item()) :: resp()

  @callback update_state!(item_id(), map()) :: resp!()
  @callback update_state(item_id(), item()) :: resp()

  @callback fast_update!(item_id(), item()) :: resp!()
  @callback fast_update(item_id(), item()) :: resp()

  @callback fast_update_state!(item_id(), map()) :: resp!()
  @callback fast_update_state(item_id(), item()) :: resp()

  @optional_callbacks fast_update_state: 2,
                      fast_update_state!: 2,
                      update: 2,
                      update!: 2,
                      update_state: 2,
                      update_state!: 2

  def get_just(res, def_ \\ nil) do
    case res do
      {:ok, res} -> res
      _ -> def_
    end
  end

  def get_maybe(nil), do: {:error, :not_found}
  def get_maybe(item), do: {:ok, item}
end
