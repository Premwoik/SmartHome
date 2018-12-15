defmodule DB.Port do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  alias DB.{Repo, Port}

  @derive {Poison.Encoder, only: [:name, :state, :id]}
  schema "ports" do
    field :name, :string
    field :number, :integer
    field :mode, :string
    field :state, :boolean
    field :pwm_fill, :integer
    field :inverted_logic, :boolean
    field :type, :string
    field :timeout, :integer
    belongs_to :device, DB.Device
  end


  def changeset(port, params \\ %{}) do
    IO.inspect(params)
    port
    |> cast(params, [:state, :number, :name, :type, :mode, :timeout])
  end

  def get(ids) do
    Repo.all from p in Port, where: p.id in ^ids, preload: [:device]
  end




  def pulse?(port) do
    port.timeout > 0
  end

  def update_state(ids, state) do
    Repo.update_all (from p in Port, where: p.id in ^ids),
                    set: [
                      state: state
                    ]
  end

  def update_state2(ports, state) do
    for port <- ports do
      Ecto.Changeset.change(port, state: state)
      |> Repo.update()
    end
  end


end
