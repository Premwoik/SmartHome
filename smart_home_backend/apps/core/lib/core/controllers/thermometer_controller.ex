defmodule Core.Controllers.ThermometerController do
  @moduledoc false

  def read(device) do
    Core.Device.do_(:read_temperatures, device)
  end
end
