defmodule DB.RfButton do
  @moduledoc false
  use Ecto.Schema
  import DB
  import Ecto.Changeset
  import Ecto.Query

  alias DB.{Repo, Device, Port, RfButton, Task, Action}

  schema "rf_buttons" do
    belongs_to(:port, Port)
    belongs_to(:action, Action)
    belongs_to(:task, Task)
    field(:name, :string)
    field(:mode, :string) # :on | :off | :toggle | :page
    field(:key_value, :string)
    field(:page_id, :integer)
    field(:ref, :integer)
  end

  def changeset(btn, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(btn, Enum.into(params, %{}), all_str)
    btn
    |> cast(params_, [:port_id, :task_id, :action_id, :name, :mode, :key_value, :page_id, :ref])
  end

  #  @doc false
  #  def changeset(rf_button, attrs) do
  #    rf_button
  #    |> cast(attrs, [:port, :action, :task, :name, :mode, :key_value])
  #    |> validate_required([:port, :action, :task, :name, :mode, :key_value])
  #  end

  def get_or_create(key) do
    res = from(b in RfButton, where: b.key_value == ^key)
          |> Repo.all()
          |> Repo.preload([port: [:device], action: [], task: []])
    if res == [] do
      [
        %RfButton{
          name: "nowy",
          mode: "toggle",
          key_value: key,
          port_id: nil,
          action_id: nil,
          task_id: nil,
          ref: 1,
          page_id: 1
        }
	# |> Repo.insert!() # TODO
      ]
    else
      res
    end
  end

  #  def get_or_create(key, page_id \\ 1) do
  #    res = Repo.get_by(RfButton, [key_value: key, page_id: page_id])
  #          |> Repo.preload([port: [:device], action: [], task: []])
  #    if is_nil(res) do
  #      %RfButton{
  #        name: "nowy",
  #        mode: "toggle",
  #        key_value: key,
  #        port_id: nil,
  #        action_id: nil,
  #        task_id: nil,
  #        ref: 1,
  #        page_id: 1
  #      }
  #      #      |> Repo.insert!() % TODO
  #    else
  #      res
  #    end
  #  end

end
