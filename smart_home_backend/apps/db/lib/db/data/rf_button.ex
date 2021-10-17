defmodule DB.Data.RfButton do
  @moduledoc """
  The rf button data.
  """
  use Ecto.Schema
  import Ecto.Changeset

  alias DB.Data.Action
  alias DB.Data.Port
  alias DB.Data.RfButton
  alias DB.MainRepo

  @type mode_t :: :on | :off | :toggle | :page

  @typedoc """
  FIXME add doc for fields
  """
  @type t :: %RfButton{
          id: integer(),
          mode: mode_t(),
          key_value: String.t(),
          page: integer(),
          on_click_action: map()
        }

  schema "rf_buttons" do
    field(:name, :string)
    field(:mode, Ecto.Enum, values: [:on, :off, :toggle, :page])
    field(:key_value, :string)
    field(:page, :integer, default: 0)
    field(:on_click_action, :map)
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :mode, :key_value, :on_click_action])
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

  def identify(key_value) do
    MainRepo.get_by(RfButton, key_value: key_value)
  end

  def list_all() do
    {:ok, list_all!()}
  end

  def list_all!() do
    MainRepo.all(RfButton)
  end
end
