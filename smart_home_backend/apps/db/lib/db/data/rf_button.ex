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
          on_click_action: map()
        }

  schema "rf_buttons" do
    field(:name, :string)
    field(:mode, Ecto.Enum, values: [:on, :off, :toggle, :page, :alias])
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

  def group_by_pilot!() do
    list_all!()
    |> Enum.group_by(
      fn p ->
        case String.split(p.name, "-") do
          [pilot, _] -> pilot
          _ -> "unknown"
        end
      end,
      fn p ->
        case String.split(p.name, "-") do
          [_, num] -> {String.to_atom(num), p}
          _ -> {"unknown", p}
        end
      end
    )
  end

  def update_action(rf_button, page, action) do
    pages =
      case rf_button.on_click_action do
        nil -> %{}
        otherwise -> Map.get(otherwise, "pages", %{})
      end

    pages = Map.put(pages, page, action)

    rf_button = Ecto.Changeset.change(rf_button, on_click_action: %{"pages" => pages})

    case MainRepo.update(rf_button) do
      {:ok, struct} -> struct
      {:error, _} -> nil
    end
  end

  def get_actions(rf_button) do
    case rf_button.on_click_action do
      nil -> %{}
      otherwise -> Map.get(otherwise, "pages", %{})
    end
    |> Enum.map(fn {page, action} ->
      case action do
        %{"id" => id, "type" => "action"} ->
          {page, DB.Proc.ActionListProc.get!(id)}

        %{"id" => id, "type" => "port"} ->
          {page, DB.Proc.PortListProc.get!(id)}
      end
    end)
  end

  def clear_actions(%RfButton{} = rf_button) do
    rf_button = Ecto.Changeset.change(rf_button, on_click_action: nil)

    case MainRepo.update(rf_button) do
      {:ok, struct} -> struct
      {:error, _} -> nil
    end
  end
end
