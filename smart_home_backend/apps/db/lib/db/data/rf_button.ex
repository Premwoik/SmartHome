defmodule DB.Data.RfButton do
  @moduledoc """
  The rf button data.
  """
  use Ecto.Schema
  import Ecto.Changeset

  alias DB.Data.Action
  alias DB.Data.Port
  alias DB.Data.RemoteController
  alias DB.Data.RfButton
  alias DB.MainRepo

  @type mode_t :: :on | :off | :toggle | :page | :alias

  @typedoc """
  FIXME add doc for fields
  """
  @type t :: %RfButton{
          id: integer(),
          name: String.t(),
          mode: mode_t(),
          key_value: String.t(),
          page: integer(),
          on_click_action: map(),
          controller: RemoteController.t()
        }

  schema "rf_buttons" do
    field(:name, :string)
    field(:mode, Ecto.Enum, values: [:on, :off, :toggle, :page, :alias])
    field(:key_value, :string)
    field(:page, :integer, default: 0)
    field(:on_click_action, :map)
    belongs_to(:controller, RemoteController)
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :key_value, :on_click_action])
  end

  @spec list_all!() :: [RfButton.t()]
  def list_all!() do
    MainRepo.all(RfButton)
  end

  @spec identify([RfButton.t()], [String.t()]) :: [RfButton.t()]
  def identify(buttons, key_values) do
    for b <- buttons, b.key_value in key_values, do: b
  end

  @spec update(%RfButton{}, map()) :: {:ok, RfButton.t()} | {:error, Ecto.Changeset.t()}
  def update(btn, params) do
    changeset(btn, params)
    |> MainRepo.update()
  end

  @spec virtual_update(%RfButton{}, map()) :: {:ok, RfButton.t()} | {:error, Ecto.Changeset.t()}
  def virtual_update(btn, params) do
    with %Ecto.Changeset{valid?: true, data: data, changes: changes} <- changeset(btn, params) do
      new_data = Map.merge(Map.from_struct(data), changes)
      {:ok, struct(%RfButton{}, new_data)}
    else
      error_changeset ->
        {:error, error_changeset}
    end
  end

  @spec click_action(RfButton.t(), integer()) :: Port.t() | Action.t() | nil
  def click_action(btn, page) do
    page_str = Integer.to_string(page)
    data = Map.get(btn, :on_click_action, %{"pages" => %{}})

    case Map.get(data["pages"], page_str) do
      %{"id" => id, "type" => "action"} ->
        DB.Proc.ActionListProc.get!(id)

      %{"id" => id, "type" => "port"} ->
        DB.Proc.PortListProc.get!(id)

      _ ->
        nil
    end
  end

  @spec get_actions(%RfButton{}) :: [{page, item}]
        when page: integer(), item: %Port{} | %Action{} | nil
  def get_actions(rf_button) do
    rf_button
    |> Map.get(:on_click_action)
    |> nil_to_empty_map()
    |> Map.get("pages", %{})
    |> Enum.map(fn {page, action} ->
      case action do
        %{"id" => id, "type" => "action"} ->
          {page, DB.Proc.ActionListProc.get!(id)}

        %{"id" => id, "type" => "port"} ->
          {page, DB.Proc.PortListProc.get!(id)}

        _ ->
          nil
      end
    end)
  end

  def nil_to_empty_map(nil), do: %{}
  def nil_to_empty_map(map), do: map

  def get_layout(rf_button) do
    rf_button
    |> Map.get(:on_click_action, %{})
    |> Map.get("layout", %{"cols" => 1, "rows" => 1})
  end

  @sep "-"

  def group_by_pilot(buttons) do
    Enum.group_by(buttons, fn %RfButton{name: name} -> hd(String.split(name, @sep)) end)
  end
end
