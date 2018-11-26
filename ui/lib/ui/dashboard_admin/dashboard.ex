defmodule Ui.DashboardAdmin.Dashboard do
  use Ecto.Schema
  import Ecto.Changeset


  schema "dashboards" do
    field :description, :string
    field :name, :string
    field :number, :integer
    field :title, :string

    timestamps()
  end

  @doc false
  def changeset(dashboard, attrs) do
    dashboard
    |> cast(attrs, [:name, :title, :description, :number])
    |> validate_required([:name, :title, :description, :number])
  end
end
