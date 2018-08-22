defmodule SocketTest2.MixProject do
  use Mix.Project

  def project do
    [
      app: :socket_test2,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {SocketTest2, []},
      extra_applications: [:logger],
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:poison, "~> 3.1"},
      {:connection, "~> 1.0.4"},
      {:db, path: "../db"}
    ]
  end
end
