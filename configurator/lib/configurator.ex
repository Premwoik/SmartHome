defmodule Configurator do
  @moduledoc """
  Documentation for Configurator.
  """
  @server Application.get_env(:configurator, :server)

  def send_fn(fun) do
    t = Task.Supervisor.async {Configurator.TaskSupervisor, :"server@192.168.2.105"}, fun
    Task.await(t)
  end

  defmacro send(x) do
    quote do
      fn -> unquote(x) end |> Configurator.send_fn()
    end
  end
end
