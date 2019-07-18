defmodule UiWeb.Controllers.ErrorHelper do
  use UiWeb, :controller

  def handling_casual_errors(conn, error) do
    case error do
      {:error, :wrong_id} ->
        send_resp(conn, 400, "action wrong id")
      err -> 
        send_resp(conn, 500, "Not handled error: #{inspect err}")
    end
  end
end
