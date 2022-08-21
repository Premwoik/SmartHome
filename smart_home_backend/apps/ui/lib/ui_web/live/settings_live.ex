defmodule UiWeb.SettingsLive do
  use UiWeb, :live_view

  alias DB.Data.ScheduleJob
  alias DB.Data.RemoteController
  # alias DB.Data.RfButton
  alias DB.Proc.PortListProc
  alias DB.Proc.ActionListProc
  alias DB.Proc.RfButtonListProc
  alias Core.Device.BasementPi

  @impl true
  def mount(_params, _session, socket) do
    categories = %{tasks: true, controllers: false, circuts: false}

    case connected?(socket) do
      true ->
        jobs = ScheduleJob.list_all!()
        controllers = prepare_controllers()
        selected = prepare_selected_buttons(controllers)

        heating_config =
          DB.MainRepo.get!(DB.Data.Device, 10)
          |> BasementPi.get_local_config()

        {:ok,
         assign(socket,
           jobs: jobs,
           controllers: controllers,
           selected: selected,
           heating_config: heating_config,
           categories: categories
         )}

      false ->
        {:ok,
         assign(socket,
           jobs: [],
           controllers: [],
           selected: %{},
           heating_config: %{},
           categories: categories
         )}
    end
  end

  ## Toggle 

  def handle_event("toggle_category", %{"value" => category}, socket) do
    cat = String.to_existing_atom(category)
    cats = Map.update!(socket.assigns[:categories], cat, fn v -> !v end)
    {:noreply, assign(socket, categories: cats)}
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

  # Circuts handle_event/3

  def handle_event("add_planned_run", %{"circut" => circut_name}, socket) do
    name = String.to_existing_atom(circut_name)
    config = socket.assigns[:heating_config]

    config = %{
      config
      | circuts:
          Map.update!(config.circuts, name, fn circut ->
            new_run = {:null, {0, 0, 0}, {0, 0, 0}}
            %{circut | planned_runs: [new_run | circut.planned_runs]}
          end)
    }

    {:noreply, assign(socket, heating_config: config)}
  end

  def handle_event("delete_planned_run", %{"circut" => circut_name, "run" => run_id}, socket) do
    name = String.to_existing_atom(circut_name)
    run_id = String.to_integer(run_id)
    config = socket.assigns[:heating_config]

    config = %{
      config
      | circuts:
          Map.update!(config.circuts, name, fn circut ->
            %{circut | planned_runs: List.delete_at(circut.planned_runs, run_id)}
          end)
    }

    {:noreply, assign(socket, heating_config: config)}
  end

  def handle_event("save_heating_config", %{"config" => config}, socket) do
    org_config = socket.assigns[:heating_config]

    config = update_config(org_config, config)

    case Core.Device.BasementPi.set_config(config.device, config) do
      :ok ->
        socket = put_flash(socket, :info, "Zapisano pomyślnie!")
        {:noreply, assign(socket, :heating_config, config)}

      _ ->
        socket = put_flash(socket, :error, "Coś poszło nie tak!")
        {:noreply, socket}
    end
  end

  ## Helpers

  defp update_config(config, data) do
    {boiler_min_temp, _} = Float.parse(data["boiler_min_temp"])

    %{
      config
      | boiler_min_temp: boiler_min_temp,
        temp_read_interval: Time.to_erl(Time.from_iso8601!(data["temp_read_interval"])),
        circuts: update_circuts(config.circuts, data["circuts"])
    }
  end

  defp update_circuts(old_circuts, new_circuts) do
    Enum.map(old_circuts, fn {name, old_circut} ->
      data = Map.get(new_circuts, Atom.to_string(name))
      {max_temp, _} = Float.parse(data["max_temp"])
      {min_temp, _} = Float.parse(data["min_temp"])

      {name,
       %{
         old_circut
         | max_temp: max_temp,
           min_temp: min_temp,
           running_duration: parse_time(data["running_duration"]),
           break_duration: parse_time(data["break_duration"]),
           planned_runs: update_planned_runs(data["runs"])
       }}
    end)
    |> Map.new()
  end

  defp update_planned_runs(nil) do
    []
  end

  defp update_planned_runs(runs) do
    Enum.map(runs, fn {_, data} ->
      days = Enum.map(Map.get(data, "days", []), &String.to_existing_atom/1)
      days = if length(days) in [0, 7], do: :null, else: days
      start_time = parse_time(data["time"])
      duration = parse_time(data["duration"])
      {days, start_time, duration}
    end)
  end

  defp parse_time(str) do
    str = if String.length(str) > 5, do: str, else: str <> ":00"
    Time.to_erl(Time.from_iso8601!(str))
  end

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
