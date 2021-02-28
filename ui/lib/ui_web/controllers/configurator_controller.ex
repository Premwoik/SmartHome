defmodule UiWeb.ConfiguratorController do
  @moduledoc false
  use UiWeb, :controller
  require Configurator
  import Configurator
  alias DB.{Port, Action, Device, RfButton, ScheduleJob, Page, Meter, Repo}
  import DB.Init, only: [foreign: 2]
  import DB.Visualize.View

  action_fallback UiWeb.FallbackController

  def execute(conn, %{"instruction" => instruction}) do
    with {:ok, ast} <- Code.string_to_quoted(instruction) do
      code = ast |> unpipe() |> Configurator.inject_update_propagation() |> Macro.to_string()
      {res, _} = Code.eval_string(code, [], __ENV__)#[], functions: [], aliases: [], requires: [], macros: [])
      res = case res do
        [%_{}|_] -> Enum.map(res, & Map.from_struct(&1) |> Map.drop([:__meta__]))
        %_{} -> Map.from_struct(res) |> Map.drop([:__meta__])
        _ -> res
      end
      json(conn, res)
    end
  end

  def unpipe(ast) do
    [{first_ast, _index} | rest_tuples] = Macro.unpipe(ast)
    Enum.reduce(rest_tuples, first_ast,
      fn {rest_ast, rest_index}, this_ast -> Macro.pipe(this_ast, rest_ast, rest_index) end)
  end

end
