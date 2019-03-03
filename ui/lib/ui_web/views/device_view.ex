defmodule UiWeb.DeviceView do
  use UiWeb, :view
  alias UiWeb.DeviceView

  def render("index.json", %{devices: devices}) do
    render_many(devices, DeviceView, "device.json")
  end

  def render("show.json", %{device: device}) do
    render_one(device, DeviceView, "device.json")
  end

  def render("device.json", %{device: device}) do
    %{id: device.id,
      name: device.name,
      ip: device.ip,
      port: device.port,
      type: device.type,
      alive: device.alive,
      '@type': "device"
    }
  end
end
