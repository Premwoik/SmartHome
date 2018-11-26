defmodule UiWeb.Dashboard.PortView do
  use UiWeb, :view
  alias UiWeb.Admin.PortView

  def render("index.json", %{dash_ports: ports}) do
    %{data: render_many(ports, PortView, "dash_port.json")}
  end

  def render("show.json", %{dash_port: port}) do
    %{data: render_one(port, PortView, "dash_port.json")}
  end

  def render("dash_port.json", %{dash_port: port}) do
    %{id: port.id,
      name: port.name,
      number: port.number,
      mode: port.mode,
      state: port.state,
      type: port.type,
      timeout: port.timeout,
      device_id: port.device_id,
      port: ""
    }
  end
end

