defmodule Core.MixProject do
  use Mix.Project

  def project do
    [
      app: :core,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Core, []},
      extra_applications: [:logger, :db, :httpotion, :tortoise, :timex]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:poison, "~> 3.1"},
      {:httpotion, "~> 3.1.0"},
      {:connection, "~> 1.0.4"},
      {:timex, "~> 3.6"},
      {:tortoise, "~> 0.9"},
      {:witchcraft, "~> 1.0"},
      {:bypass, "~> 2.1", only: :test},
      {:quantum, "~> 3.0"},
      {:ring_logger, "~> 0.8.1", only: :dev},
      {:db, path: "../db"}
    ]
  end
end
