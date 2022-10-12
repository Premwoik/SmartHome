defmodule DB.MainRepo.Migrations.AddBasicTables do
  use Ecto.Migration

  def change do
    create table("devices") do
      add :name, :string
      add :ip, :string
      add :port, :integer
      add :type, :string
      add :status, :string
    end

    create table("ports") do
      add :name, :string
      add :number, :integer
      add :device_id, references("devices")
      add :mode, :string
      add :type, :string
      add :state, :map
    end

    create table("actions") do
      add :name, :string
      add :attributes, :map
      add :start_time, :time
      add :end_time, :time
      add :pause, :integer
      add :activator_id, references("ports")
    end

    #create table("action_ports", primary_key: false) do
      #add :action_id, references("actions")
      #add :port_id, references("ports")
    #end
    create table("schedule_jobs") do
      add :name, :string
      add :expr, :string
      add :extended, :boolean
      add :status, :boolean
      add :task, :map
    end

    create table("rf_buttons") do
      add :name, :string
      add :mode, :string
      add :key_value, :boolean
      add :page, :integer
      add :on_click_action, :map
    end

    create table("meters") do
      add :name, :string
      add :address, :string
      add :device_id, references("devices")
      add :type, :string
    end

    create table("pages") do
      add :name, :string
      add :description, :string
      add :order, :integer
    end
    
    create table("page_ports") do
      add :page_id, references("pages")
      add :port_id, references("ports")
    end

    create table("page_actions") do
      add :page_id, references("pages")
      add :action_id, references("actions")
    end

    create table("page_devices") do
      add :page_id, references("pages")
      add :device_id, references("devices")
    end

    create table("page_meters") do
      add :page_id, references("pages")
      add :meter_id, references("meters")
    end
  end
end
