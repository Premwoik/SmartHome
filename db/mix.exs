defmodule Db.MixProject do
  use Mix.Project

  def project do
    [
      app: :db,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      #      applications: [],
      extra_applications: [:logger, :memento],
      mod: {DB.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:poison, "~> 3.1"},
      {:ecto, "~> 2.0"},
      {:postgrex, "~> 0.11"},
      {:memento, "~> 0.3.1"},
      {:timex, "~> 3.6"}
    ]
  end
end
