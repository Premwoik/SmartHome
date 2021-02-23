defmodule DB.CRUD do
  @moduledoc false

  require Logger

  @type id :: integer
  @type foreign(module) :: {:foreign, module, integer}
  @type ref :: integer

  @callback all() :: list(struct)
  @callback get() :: {:ok, struct}
  @callback find({:atom, any, any}) :: {:ok, list(struct)} | any
  @callback insert(struct) :: {:ok, struct} | any
  @callback update(struct) :: {:ok, struct} | any
  @callback update(struct, map) :: {:ok, struct} | any

  #  @callback all_q() :: list(struct)
  #  @callback get_q() :: {:ok, struct}
  #  @callback find_q({:atom, any, any}) :: {:ok, list(struct)} | any
  #  @callback insert_q(struct) :: {:ok, struct} | any

  defmacro __using__(opts) do
    quote do
      @opts unquote(opts)
      alias DB.CRUD

      def all() do
        Memento.transaction(fn ->
          Memento.Query.all(__MODULE__)
        end)
        |> CRUD.unwrap()
      end

      def short_info() do
        all() |> Enum.map(& {&1.id, &1.name})
      end

      def get(ids) when is_list(ids) do
        Memento.transaction(fn ->
          Enum.map(ids, &get_q/1)
        end)
        |> CRUD.unwrap()
      end

      def get(id) do
        Memento.transaction(fn ->
          get_q(id)
        end)
        |> CRUD.unwrap()
      end

      def get_q(id) do
        Memento.Query.read(__MODULE__, id)
      end

      def insert(items) when is_list(items) do
        Memento.transaction(fn ->
          Enum.map(items, &Memento.Query.write/1)
        end)
        |> CRUD.unwrap()
      end

      def insert(item) do
        with :ok <- validate(item) do
          Memento.transaction(fn ->
            Memento.Query.write(item)
          end)
          |> CRUD.unwrap()
        end
      end

      def update(items) when is_list(items) do
        Memento.transaction(fn ->
          Enum.map(items, &update_q/1)
        end)
        |> CRUD.unwrap()
      end

      def update(%{__meta__: Memento.Table} = item) do
        Memento.transaction(fn ->
          update_q(item)
        end)
        |> CRUD.unwrap()
      end

      defp update_q(%{__meta__: Memento.Table, id: id} = item) do
        with :ok <- validate(item),
             actual <- Memento.Query.read(__MODULE__, id),
             :ok <- CRUD.check_ref(actual, item) do
          CRUD.inc_ref(item)
          |> Memento.Query.write()
        end
      end

      def cast(items, params) when is_list(items) do
        Enum.map(items, fn i -> cast(i, params) end)
      end

      def cast(%{} = item, params) do
        Enum.reduce(params, item, fn {key, val}, acc ->
          key = if(is_binary(key), do: String.to_atom(key), else: key)
          Map.put(acc, key, deep_cast(item, key, val))
        end)
      end

      def deep_cast(item, key, val) do
        case Map.get(item, key) do
          %{} = i -> cast(i, val)
          _ -> val
        end
      end

      def update(item, params) do
        cast(item, params)
        |> update()
      end

      def find(guards) do
        Memento.transaction(fn ->
          Memento.Query.select(__MODULE__, guards)
        end)
        |> CRUD.unwrap()
      end

      def find_raw(match_head \\ & &1, guards \\ [], result \\ [:"$_"]) do
        match_head = match_head.(__MODULE__.__info__().query_base)

        Memento.transaction(fn ->
          Memento.Query.select_raw(__MODULE__, [{match_head, guards, result}])
        end)
        |> CRUD.unwrap()
      end

      #      @spec preload(struct, boolean | keyword) :: struct
      #      def preload(item), do: item

      def new(params \\ []) do
        default = Keyword.get(@opts, :default, [])
        params = Keyword.merge(default, params)
        struct(__MODULE__, params)
      end

      @spec validate(struct) :: :ok | {:error, String.t()}
      def validate(item) do
        :ok
      end

      defoverridable validate: 1
    end
  end

  def unwrap({:ok, r}), do: r

  def unwrap(e) do
    Logger.error("Error #{inspect(e)} occured when unwraping result.")
    nil
  end

  def check_ref(%{ref: db_ref} = db, %{ref: new_ref} = new) do
    if db_ref == new_ref do
      :ok
    else
      if db == Map.put(new, :ref, db_ref) do
        db
      else
        {:error, "Old version of object"}
      end
    end
  end

  def inc_ref(%{ref: ref} = obj) do
    ref = if(is_nil(ref), do: 1, else: ref + 1)
    Map.put(obj, :ref, ref)
  end
end
