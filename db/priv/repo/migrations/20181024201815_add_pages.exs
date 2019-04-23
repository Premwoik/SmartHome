defmodule DB.Repo.Migrations.AddPages do
  use Ecto.Migration

  def change do

    create table(:pages) do
      add :name, :string
      add :title, :string, default: ""
      add :description, :string, default: ""
      add :order, :integer, default: 100
    end

    create table(:page_content_lights) do
      add :page_id, references(:pages, on_delete: :delete_all)
      add :light_id, references(:lights, on_delete: :delete_all)
      add :order, :integer, default: 100
    end

    create table(:page_content_dimmers) do
      add :page_id, references(:pages, on_delete: :delete_all)
      add :dimmer_id, references(:dimmers, on_delete: :delete_all)
      add :order, :integer, default: 100
    end


    create table(:page_content_ports) do
      add :page_id, references(:pages, on_delete: :delete_all)
      add :port_id, references(:ports, on_delete: :delete_all)
      add :order, :integer, default: 100
    end

    create table(:page_content_sunblinds) do
      add :page_id, references(:pages, on_delete: :delete_all)
      add :sunblind_id, references(:sunblinds, on_delete: :delete_all)
      add :order, :integer, default: 100
    end

    create table(:page_content_actions) do
      add :page_id, references(:pages, on_delete: :delete_all)
      add :action_id, references(:actions, on_delete: :delete_all)
      add :order, :integer, default: 100
    end

    create table(:page_content_tasks) do
      add :page_id, references(:pages, on_delete: :delete_all)
      add :task_id, references(:tasks, on_delete: :delete_all)
      add :order, :integer, default: 100
    end

    create table(:page_content_devices) do
      add :page_id, references(:pages, on_delete: :delete_all)
      add :device_id, references(:devices, on_delete: :delete_all)
      add :order, :integer, default: 100
    end
  end
end
