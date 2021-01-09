defmodule UiWeb.DimmerController do
  use UiWeb, :controller

  alias Ui.DimmerAdmin, as: Admin
  alias Core.Controllers.DimmerController, as: Controller
  alias DB.{Dimmer}

  action_fallback(UiWeb.FallbackController)

  #  CRUD

  def index(conn, _params) do
    dimmers = Admin.list_dimmers()
    render(conn, "index.json", dimmers: dimmers)
  end

  def create(conn, %{"dimmer" => dimmer_params}) do
    with {:ok, %Dimmer{} = dimmer} <- Admin.create_dimmer(dimmer_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.dimmer_path(conn, :show, dimmer))
      |> render("show.json", dimmer: dimmer)
    end
  end

  def show(conn, %{"id" => id}) do
    dimmer = Admin.get_dimmer!(id)
    render(conn, "show.json", dimmer: dimmer)
  end

  def update(conn, %{"id" => id, "dimmer" => dimmer_params}) do
    dimmer = Admin.get_dimmer!(id)
    # TODO remove below mock
    dimmer_params2 = Map.put(dimmer_params, "time", Map.get(dimmer_params, "full_time"))

    with {:ok, %Dimmer{} = dimmer} <- Admin.update_dimmer(dimmer, dimmer_params2) do
      render(conn, "show.json", dimmer: dimmer)
    end
  end

  def delete(conn, %{"id" => id}) do
    dimmer = Admin.get_dimmer!(id)

    with {:ok, %Dimmer{}} <- Admin.delete_dimmer(dimmer) do
      send_resp(conn, :no_content, "")
    end
  end

  #  CUSTOM

  def set_on(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "state", true))
  end

  def set_off(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "state", false))
  end

  @doc """
    Set function can be used for: set_color, set_state, set_brightness or set_white_brightness.

    The object map keys combinations for following actions:
     - set_color ->  %{red: integer(), green: integer(), blue: integer()}
     - set_state ->  %{state: bool()}
     - set_brightness ->  %{fill: integer()}
     - set_white_brightness ->  %{white: integer()}

  """


  def set(conn, %{"id" => id, "state" => state} = o) do
    set_state = if(state, do: &Controller.turn_on/1, else: &Controller.turn_off/1)

    with {:ok, dim} <- Admin.get_dimmer(id),
         true <- DB.check_ref(o, dim),
         :ok <- Benchmark.measure_p(fn -> set_state.(dim) end),
         do: succ_return(conn, id)
  end

  def set(conn, %{"id" => id, "red" => r, "green" => g, "blue" => b} = o) do
    with {:ok, dim} <- Admin.get_dimmer(id),
         true <- DB.check_ref(o, dim),
         :ok <- Benchmark.measure_p(fn -> Controller.set_color(dim, r, g, b) end),
         do: succ_return(conn, id)
  end

  def set(conn, %{"id" => id, "fill" => fill} = o) do
#    fill = fill_guard(fill)

    with {:ok, dim} <- Admin.get_dimmer(id),
         true <- DB.check_ref(o, dim),
         :ok <- Benchmark.measure_p(fn -> Controller.set_brightness(dim, fill) end),
         do: succ_return(conn, id)
  end

  def set(conn, %{"id" => id, "white" => fill} = o) do
#    fill = fill_guard(fill)

    with {:ok, dim} <- Admin.get_dimmer(id),
         true <- DB.check_ref(o, dim),
         :ok <- Benchmark.measure_p(fn -> Controller.set_white_brightness(dim, fill) end),
         do: succ_return(conn, id)
  end

  # Privates

  defp succ_return(conn, id) do
    dim = Admin.get_dimmer!(id)
    render(conn, "show.json", dimmer: dim)
  end

  defp fill_guard(fill) do
    cond do
      fill > 100 -> 100
      fill < 0 -> 0
      true -> fill
    end
  end
end
