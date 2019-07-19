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
      applications: [:timex],
      extra_applications: [:logger, :db, :httpotion],
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:mock, "~> 0.3.0", only: :test},
      {:mox, "~> 0.4", only: :test},
      {:poison, "~> 3.1"},
      {:httpotion, "~> 3.1.0"},
      {:connection, "~> 1.0.4"},
      {:timex, "~> 3.0"},
      {:db, path: "../db"},
#      {:ui, path: "../ui"}
    ]
  end
end
