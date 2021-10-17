defmodule SmartHomeBackend.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      version: "0.1.0",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      dialyzer: [plt_add_deps: :transitive]
    ]
  end

  def application do
    []
  end

  # Dependencies listed here are available only for this
  # project and cannot be accessed from applications inside
  # the apps folder.
  #
  # Run "mix help deps" for examples and options.
  defp deps do
    [
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:gradualizer_ex, github: "Premwoik/gradualizer-ex", ref: "master"},
      {:erlang_doctor,
       github: "chrzaszcz/erlang_doctor", only: [:dev], manage: :rebar3, ref: "master"}
    ]
  end

  defp aliases do
    [
      test: "test --no-start"
    ]
  end
end