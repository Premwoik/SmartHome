defmodule DB.Action do
  @moduledoc false
  use Ecto.Schema

  schema "actions" do
    field :function, :string
    field :active, :boolean
    field :params, :string
    belongs_to :port, DB.Port
    many_to_many :args, DB.Port, join_through: "actions_arguments", on_delete: :delete_all
  end



end
