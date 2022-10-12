defmodule HomeUi.Repo do
  use Ecto.Repo,
    otp_app: :home_ui,
    adapter: Ecto.Adapters.Postgres
end
