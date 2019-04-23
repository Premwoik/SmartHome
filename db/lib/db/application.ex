defmodule DB.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @otp_app Mix.Project.config()[:app]
  def start(type, _args) do
    # List all child processes to be supervised
    setup_db!(type)

    children = [
      DB.Repo
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: DB.Supervisor]
    res = Supervisor.start_link(children, opts)
    #    IO.inspect(type)
    if type != :test, do: DB.Init.run()
    res
  end

  defp setup_db!(type) do
    repos = Application.get_env(@otp_app, :ecto_repos)

    for repo <- repos do
      if Application.get_env(@otp_app, repo)[:adapter] == Elixir.Sqlite.Ecto2 do
        if type == :test, do: drop_repo!(repo)
        setup_repo!(repo)
        migrate_repo!(repo)
      end
    end

    :ok
  end

  defp setup_repo!(repo) do
    db_file = Application.get_env(@otp_app, repo)[:database]

    unless File.exists?(db_file) do
      :ok = repo.__adapter__.storage_up(repo.config)
    end
  end

  defp migrate_repo!(repo) do
    opts = [all: true]
    {:ok, pid, apps} = Mix.Ecto.ensure_started(repo, opts)

    migrator = &Ecto.Migrator.run/4
    pool = repo.config[:pool]
    migrations_path = Path.join([:code.priv_dir(@otp_app) |> to_string, "repo", "migrations"])

    migrated =
      if function_exported?(pool, :unboxed_run, 2) do
        pool.unboxed_run(repo, fn -> migrator.(repo, migrations_path, :up, opts) end)
      else
        IO.inspect(repo)
        IO.inspect(migrations_path)
        IO.inspect(repo)
        migrator.(repo, migrations_path, :up, opts)
      end

    pid && repo.stop(pid)
    Mix.Ecto.restart_apps_if_migrated(apps, migrated)
  end

  defp drop_repo!(repo), do: Mix.Tasks.Ecto.Drop.run(["-r", repo])
end
