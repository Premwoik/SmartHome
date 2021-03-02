defmodule UiWeb.MeterView do
  use UiWeb, :view
  alias UiWeb.{MeterView, MeterReadingsView}
  import UiWeb.View.Helper

  def render("index.json", %{meters: meters}) do
    render_many(meters, MeterView, "meter.json")
  end

  def render("show.json", %{meter: meter}) do
    render_one(meter, MeterView, "meter.json")
  end

  def render("meter.json", %{meter: meter}) do
    %{
      id: meter.id,
      device_id: foreign_view(meter.device_id),
      name: meter.name,
      address: meter.address,
      readings: objs_to_view(MeterReadingsView, :meter_reading, meter.readings),
      type: meter.type,
      ref: meter.ref,
      "@type": "meter"
    }
  end
end
