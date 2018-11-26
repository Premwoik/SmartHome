defmodule UiWeb.PortView do
  use UiWeb, :view
  alias UiWeb.PortView



  def render("index.json", %{ports: ports}) do
    %{data: render_many(ports, PortView, "port.json")}
  end

  def render("show.json", %{port: port}) do
    %{data: render_one(port, PortView, "port.json")}
  end

  def render("port.json", %{port: port}) do
    %{id: port.id,
      name: port.name,
      number: port.number,
      mode: port.mode,
      state: port.state,
      type: port.type,
      timeout: port.timeout,
      device_id: port.device_id}
  end

  def render("index.json", %{dash_ports: ports}) do
    %{data: render_many(ports, PortView, "dash_port.json")}
  end

  def render("show.json", %{dash_port: port}) do
    %{data: render_one(port, PortView, "dash_port.json")}
  end

  def render("dash_port.json", %{port: port}) do
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
