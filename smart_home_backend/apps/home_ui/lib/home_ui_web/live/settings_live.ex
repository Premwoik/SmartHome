defmodule HomeUiWeb.SettingsLive do
  use HomeUiWeb, :live_view

  alias DB.Data.ScheduleJob

  def mount(_params, _session, socket) do
    jobs = ScheduleJob.list_all!()

    {:ok, assign(socket, jobs: jobs)}
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

  def update(jobs, %{id: id} = job) do
    Enum.map(jobs, fn
      %{id: id2} when id2 == id ->
        job

      j ->
        j
    end)
  end
end
