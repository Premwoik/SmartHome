defmodule UiWeb.SettingsLive do
  use UiWeb, :live_view

  alias DB.Data.ScheduleJob
  alias DB.Data.RfButton
  alias DB.Proc.PortListProc
  alias DB.Proc.ActionListProc

  def mount(_params, _session, socket) do
    jobs = ScheduleJob.list_all!()
    pilots = RfButton.group_by_pilot!()
    items = ActionListProc.list_all!()

    selected_btn =
      Enum.map(pilots, fn {k, _} -> {k, %{"items" => items}} end)
      |> Map.new()

    {:ok, assign(socket, jobs: jobs, pilots: pilots, selected: selected_btn)}
  end

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

  def handle_event(
        "rf_button_change",
        %{"button" => %{"type" => type, "pilot" => pilot, "page" => page, "mode" => mode}},
        socket
      ) do
    items =
      case type do
        "action" ->
          ActionListProc.list_all!()

        "port" ->
          PortListProc.list_all!()

        "" ->
          []
      end

    selected = socket.assigns[:selected]
    btn_mem = Map.get(selected, pilot, %{})
    btn_mem = Map.put(btn_mem, "type", type)
    btn_mem = Map.put(btn_mem, "items", items)
    btn_mem = Map.put(btn_mem, "page", page)
    btn_mem = Map.put(btn_mem, "mode", mode)
    selected = Map.put(selected, pilot, btn_mem)

    {:noreply, assign(socket, selected: selected)}
  end

  def handle_event(
        "rf_button_submit",
        %{
          "button" =>
            %{"pilot" => _pilot, "type" => type, "page" => page, "item_id" => item_id} = params
        },
        socket
      ) do
    socket =
      case Map.get(params, "pilot") do
        nil ->
          put_flash(socket, :error, "Przycisk nie został wybrany!")

        pilot ->
          selected = socket.assigns[:selected]
          pilots = socket.assigns[:pilots]
          data = Map.fetch!(selected, pilot)
          id = String.to_atom(Map.fetch!(data, "id"))

          {item_id, ""} = Integer.parse(item_id)

          pilot_d = Map.fetch!(pilots, pilot)
          btn = Keyword.fetch!(pilot_d, id)
          new_btn = RfButton.update_action(btn, page, %{"type" => type, "id" => item_id})
          new_pilot = Keyword.put(pilot_d, id, new_btn)
          new_pilots = Map.put(pilots, pilot, new_pilot)

          assign(socket, :pilots, new_pilots)
      end

    {:noreply, socket}
  end

  def handle_event("select_btn", %{"pilot" => pilot, "id" => id}, socket) do
    selected = socket.assigns[:selected]
    btn_mem = Map.get(selected, pilot, %{})
    btn_mem = Map.put(btn_mem, "id", id)
    selected = Map.put(selected, pilot, btn_mem)

    {:noreply, assign(socket, selected: selected)}
  end

  def handle_event("clear_btn", %{"pilot" => pilot, "id" => id}, socket) do
    pilots = socket.assigns[:pilots]
    pilot_btns = Map.get(pilots, pilot, [])
    id = String.to_atom(id)
    {:ok, %RfButton{} = btn} = Keyword.fetch(pilot_btns, id)
    btn = RfButton.clear_actions(btn)
    pilot_btns = Keyword.put(pilot_btns, id, btn)
    pilots = Map.put(pilots, pilot, pilot_btns)

    {:noreply, assign(socket, pilots: pilots)}
  end

  def update(jobs, %{id: id} = job) do
    Enum.map(jobs, fn
      %{id: id2} when id2 == id ->
        job

      j ->
        j
    end)
  end
end
