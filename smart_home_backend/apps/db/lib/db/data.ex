defmodule DB.Data do
  @type t :: struct()
  @type id :: integer()

  @callback list_all() :: {:ok, [t()]}
  @callback list_all!() :: [t()]

  #@callback get(id :: id()) :: {:ok, t()} || {:error, term()}
  #@callback update(item:: id()) :: {:ok, t()} || {:error, term()}
  #@callback get(id :: id()) :: {:ok, t()} || {:error, term()}
end
