defmodule Core.HandyConfig do
  @ets_registry :core_dynamic_config

  @broadcast_handler_key :broadcast_handler

  def init() do
    :ets.new(@ets_registry, [:bag, :named_table, :public])
    :ok
  end

  def register_broadcast_handler(module) do
    true = :ets.insert(@ets_registry, {@broadcast_handler_key, module})
    :ok
  end

  @spec get_broadcast_handlers() :: [module()]
  def get_broadcast_handlers() do
    :ets.lookup(@ets_registry, @broadcast_handler_key)
    |> Enum.map(&elem(&1, 1))
  end
end
