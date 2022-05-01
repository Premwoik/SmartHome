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
      extra_applications: [:logger, :httpotion, :tortoise, :timex]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:telemetry_metrics, "~> 0.4"},
      {:telemetry_poller, "~> 1.0"},
      {:phoenix_pubsub, "~> 2.0"},
      {:mock, "~> 0.3.0", only: :test},
      {:mox, "~> 0.4", only: :test},
      {:poison, "~> 3.1"},
      {:httpotion, "~> 3.1.0"},
      {:connection, "~> 1.0.4"},
      {:timex, "~> 3.6"},
      {:tortoise, "~> 0.9"},
      {:quantum, "~> 3.0"},
      {:cachex, "~> 3.4"},
      {:circuits_gpio, "~> 0.4"},
      {:basement_core, github: "Premwoik/basement-core", runtime: false},
      {:db, in_umbrella: true}
    ]
  end
end
