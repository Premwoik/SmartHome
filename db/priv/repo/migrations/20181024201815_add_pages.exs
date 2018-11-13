defmodule DB.Repo.Migrations.AddPages do
  use Ecto.Migration

  def change do

    create table(:pages) do
      add :name, :string
      add :title, :string, default: ""
      add :description, :string, default: ""
      add :number, :integer
      add :parent_id, references(:pages, on_delete: :delete_all)
    end

    create table(:page_contents) do
      add :page_id, references(:pages, on_delete: :delete_all)
      add :number, :integer
    end

    create table(:page_content_lights, primary_key: false) do
      add :page_content_id, references(:page_contents, on_delete: :delete_all)
      add :light_id, references(:lights, on_delete: :delete_all)
      add :order, :integer, default: 0
    end

    create table(:page_content_dimmers, primary_key: false) do
      add :page_content_id, references(:page_contents, on_delete: :delete_all)
      add :dimmer_id, references(:dimmers, on_delete: :delete_all)
      add :order, :integer, default: 0
    end


    create table(:page_content_ports, primary_key: false) do
      add :page_content_id, references(:page_contents, on_delete: :delete_all)
      add :port_id, references(:ports, on_delete: :delete_all)
      add :order, :integer, default: 0
    end

    create table(:page_content_sunblinds, primary_key: false) do
      add :page_content_id, references(:page_contents, on_delete: :delete_all)
      add :sunblind_id, references(:sunblinds, on_delete: :delete_all)
      add :order, :integer, default: 0
    end

    create table(:page_content_actions, primary_key: false) do
      add :page_content_id, references(:page_contents, on_delete: :delete_all)
      add :action_id, references(:actions, on_delete: :delete_all)
      add :order, :integer, default: 0
    end

    create table(:page_content_tasks, primary_key: false) do
      add :page_content_id, references(:page_contents, on_delete: :delete_all)
      add :task_id, references(:tasks, on_delete: :delete_all)
      add :order, :integer, default: 0
    end

  end
end
