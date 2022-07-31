defmodule UiWeb.SettingsLive do
  use UiWeb, :live_view

  alias DB.Data.ScheduleJob
  alias DB.Data.RemoteController
  alias DB.Data.RfButton
  alias DB.Proc.PortListProc
  alias DB.Proc.ActionListProc
  alias DB.Proc.RfButtonListProc

  @impl true
  def mount(_params, _session, socket) do
    jobs = ScheduleJob.list_all!()
    controllers = prepare_controllers()
    selected = prepare_selected_buttons(controllers)
    {:ok, assign(socket, jobs: jobs, controllers: controllers, selected: selected)}
  end

  ## Tasks handle_event/3

  @impl true
  def handle_event("save", %{"job" => %{"id" => id} = params}, socket) do
    params = Map.merge(%{"status" => "false", "extended" => "false"}, params)

    with {id, ""} <- Integer.parse(id),
         {:ok, %ScheduleJob{} = job} <- ScheduleJob.update(id, params) do
      :ok = Core.Scheduler.reload_job_from_db(id)
      jobs = update(socket.assigns[:jobs], job)

      socket = put_flash(socket, :info, "Zapisano pomyślnie!")
      {:noreply, assign(socket, jobs: jobs)}
    else
      _ ->
        socket = put_flash(socket, :error, "Coś poszło nie tak!")
        {:noreply, socket}
    end
  end

  def handle_event("run", %{"value" => id}, socket) do
    with {id, ""} <- Integer.parse(id),
         %ScheduleJob{} = job <- ScheduleJob.get!(id) do
      name = String.to_existing_atom(job.name)
      :ok = Core.Scheduler.Ctl.run_job(name)
    end

    socket = put_flash(socket, :info, "Akcja została wywołana pomyślnie!")
    {:noreply, socket}
  end

  ## Remote controllers handle_event/3

  def handle_event("rf_button_change", %{"button" => button}, socket) do
    %{"id" => id, "type" => type, "pilot" => pilot, "page" => page, "mode" => mode} = button
    id = if(id != "", do: String.to_integer(id), else: nil)
    pilot_id = String.to_integer(pilot)
    btn_mem = %{id: id, type: type, items: prepare_items(type), page: page, mode: mode}
    selected = Map.put(socket.assigns[:selected], pilot_id, btn_mem)

    {:noreply, assign(socket, selected: selected)}
  end

  def handle_event("rf_button_submit", %{"button" => button}, socket) do
    %{"id" => id, "type" => type, "page" => page, "item_id" => item_id} = button

    case id do
      nil ->
        {:noreply, put_flash(socket, :error, "Przycisk nie został wybrany!")}

      _ ->
        id = String.to_integer(id)
        item_id = String.to_integer(item_id)
        RfButtonListProc.update_action!(id, page, %{"type" => type, "id" => item_id})

        {:noreply, assign(socket, :controllers, prepare_controllers())}
    end
  end

  def handle_event("rf_button_select", %{"pilot" => pilot, "id" => id}, socket) do
    id = String.to_integer(id)
    pilot_id = String.to_integer(pilot)
    selected = put_in(socket.assigns[:selected], [pilot_id, :id], id)
    {:noreply, assign(socket, selected: selected)}
  end

  def handle_event("rf_button_clear", %{"id" => id}, socket) do
    id = String.to_integer(id)
    RfButtonListProc.update(id, %{on_click_action: %{"pages" => %{}}})
    {:noreply, assign(socket, controllers: prepare_controllers())}
  end

  ## Helpers

  defp update(items, %{id: id} = item) do
    Enum.map(items, fn
      %{id: ^id} -> item
      i -> i
    end)
  end

  defp default_btn_mem() do
    %{id: nil, items: prepare_items("action"), type: "action", page: "1", mode: "action"}
  end

  defp prepare_controllers() do
    for %RemoteController{} = c <- RfButtonListProc.list_all_controllers!() do
      %{cols: cols, buttons: btns} = c
      %RemoteController{c | buttons: Enum.chunk_every(btns, cols)}
    end
  end

  defp prepare_selected_buttons(controllers) do
    btn_mem = default_btn_mem()

    controllers
    |> Enum.map(fn %RemoteController{id: id} -> {id, btn_mem} end)
    |> Map.new()
  end

  defp prepare_items("action"), do: ActionListProc.list_all!() |> format_items() 
  defp prepare_items("port"), do: PortListProc.list_all!() |> format_items()
  defp prepare_items(_), do: []

  defp format_items(items) do
    items |> Enum.sort_by(& &1.name) |> Enum.map(&{&1.name, &1.id})
  end
end
