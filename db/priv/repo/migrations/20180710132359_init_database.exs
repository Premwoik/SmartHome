defmodule DB.Repo.Migrations.InitDatabase do
  use Ecto.Migration

  def change do
    create table(:devices) do
      add :name, :string
      add :ip, :string
      add :port, :integer
      #add :type, :string
      add :alive, :boolean
      add :process, :boolean
    end
    create table(:ports) do
      add :device_id, references("devices", on_delete: :nilify_all)
      add :name, :string
      add :type, :string
      add :number, :integer
      add :mode, :string
      add :timeout, :integer, default: 0
      add :state, :boolean
      add :pwm_fill, :integer, default: 0
      add :inverted_logic, :boolean, default: false
    end
    create table(:watchers) do
      add :device_id, references("devices", on_delete: :nilify_all)
      add :status, :boolean
      add :freq, :integer
    end

    create table(:actions) do
      add :name, :string
      add :active, :boolean
      add :params, :string
      add :function, :string
      add :port_id, references("ports", on_delete: :nilify_all)
    end
    create table(:actions_arguments, primary_key: false) do
      add :action_id, references("actions", on_delete: :delete_all)
      add :port_id, references("ports", on_delete: :delete_all)
    end

#    create table(:dimmers_lights, primary_key: false) do
#      add :dimmer_id, references("dimmers")
#      add :port_id, references("ports")
#    end

    create table(:dimmers) do
      add :port_id, references("ports", on_delete: :nilify_all)
      add :type, :string
      add :fill, :integer
      add :direction, :integer, default: 1
      add :time, :integer, default: 4_500
    end
  end
end
