defmodule DB.MainRepo do
  use Ecto.Repo, otp_app: :db, adapter: Ecto.Adapters.Postgres

  @moduledoc false
end
