defmodule DB.Repo.Migrations.InitDatabase do
  use Ecto.Migration

  def change do
    create table(:devices) do
      add :name, :string
      add :ip, :string
      add :port, :integer
      add :type, :string
    end
    create table(:ports) do
      add :device_id, references("devices")
      add :name, :string
      add :type, :string
      add :number, :integer
      add :mode, :string
      add :timeout, :integer, default: 0
      add :state, :boolean
    end
    create table(:watchers) do
      add :device_id, references("devices")
      add :status, :boolean
      add :freq, :integer
    end
    create table(:actions) do
      add :active, :boolean
      add :params, :string
      add :function, :string
      add :port_id, references("ports")
    end
    create table(:actions_arguments, primary_key: false) do
      add :action_id, references("actions")
      add :port_id, references("ports")
    end

    create table(:dimmers_lights, primary_key: false) do
      add :dimmer_id, references("dimmers")
      add :port_id, references("ports")
    end

    create table(:dimmers, primary_key: false) do
      add :id, references("ports"), primary_key: true
      add :fill, :integer
      add :direction, :integer, default: 1
      add :time, :integer, default: 4_500
    end
  end
end
