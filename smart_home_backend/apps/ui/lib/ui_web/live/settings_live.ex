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

    device = DB.MainRepo.get!(DB.Data.Device, 10)
    config = Core.Device.BasementPi.get_local_config(device)

    categories = %{tasks: true, pilots: false, circuts: false}

    {:ok,
     assign(socket,
       jobs: jobs,
       pilots: pilots,
       selected: selected_btn,
       heating_config: config,
       hc_device: device,
       categories: categories
     )}
  end

  def handle_event("toggle_category", %{"value" => category}, socket) do
    cat = String.to_existing_atom(category)
    cats = Map.update!(socket.assigns[:categories], cat, fn v -> !v end)
    {:noreply, assign(socket, categories: cats)}
  end

  # Tasks handle_event {{{

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

  # }}}
  # RfButton handle_event {{{

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

  # }}}
  # Circuts handle_event {{{

  def handle_event("add_run", %{"circut" => circut_id}, socket) do
    circut_id = String.to_integer(circut_id)
    IO.inspect(circut_id)
    config = socket.assigns[:heating_config]

    config =
      %{
        config
        | circuts:
            List.update_at(config.circuts, circut_id, fn circut ->
              new_run = {:null, {0, 0, 0}, {0, 0, 0}}
              %{circut | planned_runs: [new_run | circut.planned_runs]}
            end)
      }
      |> IO.inspect()

    {:noreply, assign(socket, heating_config: config)}
  end

  def handle_event("delete_run", %{"circut" => circut_id, "run" => run_id}, socket) do
    circut_id = String.to_integer(circut_id)
    run_id = String.to_integer(run_id)
    config = socket.assigns[:heating_config]

    config = %{
      config
      | circuts:
          List.update_at(config.circuts, circut_id, fn circut ->
            %{circut | planned_runs: List.delete_at(circut.planned_runs, run_id)}
          end)
    }

    {:noreply, assign(socket, heating_config: config)}
  end

  def handle_event("save_config", %{"config" => config}, socket) do
    org_config = socket.assigns[:heating_config]
    device = socket.assigns[:hc_device]

    config = update_config(org_config, config)
    :ok = Core.Device.BasementPi.set_config(device, config)

    {:noreply, socket}
  end

  # }}}

  # Internal

  def update_config(config, data) do
    %{
      config
      | boiler_min_temp: String.to_float(data["boiler_min_temp"]),
        temp_read_interval: Time.to_erl(Time.from_iso8601!(data["temp_read_interval"])),
        circuts: update_circuts(config.circuts, data["circuts"])
    }
  end

  def update_circuts(conf_circuts, circuts) do
    Enum.map(conf_circuts, fn circut ->
      data = Map.get(circuts, Atom.to_string(circut.name))

      %{
        circut
        | max_temp: String.to_float(data["max_temp"]),
          min_temp: String.to_float(data["min_temp"]),
          running_duration: parse_time(data["running_duration"]),
          break_duration: parse_time(data["break_duration"]),
          planned_runs: update_planned_runs(data["runs"])
      }
    end)
  end

  def update_planned_runs(nil) do
    []
  end

  def update_planned_runs(runs) do
    Enum.map(runs, fn {_, data} ->
      days = Enum.map(Map.get(data, "days", []), &String.to_existing_atom/1)
      days = if days == [], do: :null, else: days
      start_time = parse_time(data["time"])
      duration = parse_time(data["duration"])
      {days, start_time, duration}
    end)
  end

  def parse_time(str) do
    str = if String.length(str) > 5, do: str, else: str <> ":00"
    Time.to_erl(Time.from_iso8601!(str))
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
