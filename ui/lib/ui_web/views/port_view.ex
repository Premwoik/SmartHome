defmodule UiWeb.PortView do
  use UiWeb, :view
  alias UiWeb.PortView
  import UiWeb.View.Helper

  def render("index.json", %{ports: ports}) do
    render_many(ports, PortView, "port.json")
  end

  def render("show.json", %{port: port}) do
    render_one(port, PortView, "port.json")
  end

  def render("port.json", %{port: port}) do
    %{
      id: port.id,
      name: port.name,
      number: port.number,
      mode: port.mode,
      state: port.state,
      type: port.type,
      timeout: port.timeout,
      inverted_logic: port.inverted_logic,
      pwm_fill: port.pwm_fill,
      device_id: foreign_view(port.device_id),
      ref: port.ref,
      "@type": "port"
    }
  end

  # def render("index.json", %{dash_ports: ports}) do
  # %{data: render_many(ports, PortView, "dash_port.json")}
  # end

  # def render("show.json", %{dash_port: port}) do
  # %{data: render_one(port, PortView, "dash_port.json")}
  # end

  # def render("dash_port.json", %{port: port}) do
  # %{id: port.id,
  # name: port.name,
  # number: port.number,
  # mode: port.mode,
  # state: port.state,
  # type: port.type,
  # timeout: port.timeout,
  # device_id: port.device_id,
  # port: ""
  # }
  # end
end
