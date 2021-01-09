defmodule UiWeb.SchemaController do
  @moduledoc false
  use UiWeb, :controller


  def get_port_schema(conn, _ops) do
    with {:ok, schema} <- read_schema("port"),
         {:ok, schema_map} <- Poison.decode(schema) do
      json(conn, schema_map)
    end
  end

  def get_light_schema(conn, ops) do
    with {:ok, schema} <- read_schema("light"),
         {:ok, schema_map} <- Poison.decode(schema) do
      preload = Map.get(ops, "preload", "false")
      result = preload_item(schema_map, "port", ["port"], preload)
      json(conn, result)
    end
  end

  def get_dimmer_schema(conn, ops) do
    with {:ok, schema} <- read_schema("dimmer"),
         {:ok, schema_map} <- Poison.decode(schema) do
      preload = Map.get(ops, "preload", "false")
      result = preload_item(schema_map, "port", ["port"], preload)
      json(conn, result)
    end
  end

  def get_sunblind_schema(conn, ops) do
    with {:ok, schema} <- read_schema("sunblind"),
         {:ok, schema_map} <- Poison.decode(schema) do
      preload = Map.get(ops, "preload", "false")
      result = preload_item(schema_map, "port", ["port", "open_port"], preload)
      json(conn, result)
    end
  end

  def get_action_schema(conn, _ops) do
    with {:ok, schema} <- read_schema("action"),
         {:ok, schema_map} <- Poison.decode(schema) do
      ready = preload_instruction(schema_map, "action")
      json(conn, ready)
    end
  end

  def get_task_schema(conn, _ops) do
    name = "task"
    with {:ok, schema} <- read_schema(name),
         {:ok, schema_map} <- Poison.decode(schema) do
      ready = preload_instruction(schema_map, name)
      json(conn, ready)
    end
  end

  def get_rf_button_schema(conn, _ops) do
    name = "rf_button"
    with {:ok, schema} <- read_schema(name),
         {:ok, schema_map} <- Poison.decode(schema) do
      ready = preload_instruction(schema_map, name)
      json(conn, ready)
    end
  end

  def get_device_schema(conn, _ops) do
    name = "device"
    with {:ok, schema} <- read_schema(name),
         {:ok, schema_map} <- Poison.decode(schema) do
      ready = preload_instruction(schema_map, name)
      json(conn, ready)
    end
  end




  # Privates

  defp read_schema(name) do
    with {:ok, res} <- File.read("priv/schemas/" <> name <> "_schema.json") do
      {:ok, res}
    else
      _ -> {:error, "Can't read schema for #{name}."}
    end
  end

  defp preload_instruction(parent, name) do
    with {:ok, res} <- File.read("priv/schemas/instruction/" <> name <> ".md") do
      Map.put(parent, "instruction", res)
    else
      _ -> parent
    end
  end

  defp preload_item(parent, item, ui_items, "true") do
    with {:ok, port_schema} <- read_schema("port"),
         {:ok, port_schema_map} <- Poison.decode(port_schema) do
      definitions = %{item => port_schema_map}
      ui_schema = Enum.reduce(ui_items, parent["ui_schema"],
        fn item, acc -> Map.put(acc, item, port_schema_map["ui_schema"]) end)
      %{parent | "definitions" => definitions, "ui_schema" => ui_schema}
    end
  end
  defp preload_item(parent, _, _, "false"), do: parent

end
