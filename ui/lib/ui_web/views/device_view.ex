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
      type_id: device.type_id,
      #TODO handle not preload 
      type: render("device_type.json", %{device_type: device.type}),
      alive: device.alive,
      '@type': "device"
    }
  end
  #DEVICE TYPE
  # TODO move to new module?
  def render("index.json", %{device_types: device_types}) do
    IO.puts("PIERD")
    render_many(device_types, DeviceView, "device_type.json", as: :device_type)
  end

  def render("show.json", %{device_type: device_type}) do
    render_one(device_type, DeviceView, "device_type.json")
  end

  def render("device_type.json", %{device_type: device_type}) do
    %{id: device_type.id,
      name: device_type.name,
      module: device_type.module
    }
  end
end
