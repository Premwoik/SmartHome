defmodule DB.RfButton do
  @moduledoc false
  use Ecto.Schema

  alias DB.{Repo, Device, Port, RfButton, Task, Action}

  schema "rf_buttons" do
    belongs_to(:port, Port)
    belongs_to(:action, Action)
    belongs_to(:task, Task)
    field(:name, :string)
    field(:mode, :string) # :on | :off | :toggle
    field(:key_value, :string)
  end

  #  def changeset(btn, params \\ %{}, all_str \\ false) do
  #    params_ = inc_ref(btn, Enum.into(params, %{}), all_str)
  #    btn
  #    |> cast(params_, [:port_id, :open_port_id, :state, :direction, :full_open_time, :type, :position, :ref])
  #

  def get_or_create(key) do
    res = Repo.get_by(RfButton, key_value: key)
          |> Repo.preload([port: [:device], action: [], task: []])
    if is_nil(res) do
      %RfButton{
        name: "nowy",
        mode: "toggle",
        key_value: key,
        port_id: nil,
        action_id: nil,
        task_id: nil
      }
#      |> Repo.insert!() % TODO
    else
      res
    end
  end

end
