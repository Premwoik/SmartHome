defmodule DB.Repo.Migrations.AddPortsActivationHistory do
  use Ecto.Migration

  def change do
    create table (:input_activations_1h) do
      add :port_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:output_activations_1h) do
      add :port_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:device_activations_1h) do
      add :device_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table (:input_activations_1day) do
      add :port_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:output_activations_1day) do
      add :port_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:device_activations_1day) do
      add :device_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table (:input_activations_1week) do
      add :port_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:output_activations_1week) do
      add :port_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:device_activations_1week) do
      add :device_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table (:input_activations_1month) do
      add :port_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:output_activations_1month) do
      add :port_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:device_activations_1month) do
      add :device_id, :integer
      add :date, :naive_datetime
      add :value, :integer
    end

    create unique_index(:input_activations_1h, [:port_id, :date])
    create unique_index(:output_activations_1h, [:port_id, :date])
    create unique_index(:device_activations_1h, [:device_id, :date])

    create unique_index(:input_activations_1day, [:port_id, :date])
    create unique_index(:output_activations_1day, [:port_id, :date])
    create unique_index(:device_activations_1day, [:device_id, :date])

    create unique_index(:input_activations_1week, [:port_id, :date])
    create unique_index(:output_activations_1week, [:port_id, :date])
    create unique_index(:device_activations_1week, [:device_id, :date])

    create unique_index(:input_activations_1month, [:port_id, :date])
    create unique_index(:output_activations_1month, [:port_id, :date])
    create unique_index(:device_activations_1month, [:device_id, :date])

  end
end
