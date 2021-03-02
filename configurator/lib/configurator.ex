defmodule Configurator do
  @moduledoc """
  Documentation for Configurator.
  """
  @server Application.get_env(:configurator, :server, node())

  def send_fn(fun) do
      t = Task.Supervisor.async {Configurator.TaskSupervisor, @server}, fun
      Task.await(t)
  end

  defmacro send(ast) do
    ast = if(is_binary(ast), do: Code.string_to_quoted!(ast), else: ast)
    ast = inject_update_propagation(ast)
    quote do
      fn -> unquote(ast) end |> Configurator.send_fn()
    end
  end

  def inject_update_propagation(ast) do
    wrap = fn x -> {{:., [], [{:__aliases__, [alias: false], [:Core]}, :propagate_item_update]}, [], [x]} end
    Macro.postwalk(ast, fn
      {{:., _, [_, :update]}, _, _} = x -> wrap.(x)
      {{:., _, [_, :insert]}, _, _} = x -> wrap.(x)
      x -> x
    end)
  end
end
