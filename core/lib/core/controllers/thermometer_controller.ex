defmodule Core.Controllers.ThermometerController do
  @moduledoc false

  @callback read_temperatures(device :: %DB.Device{}) :: any

  def read(device) do
    Core.Device.do_(:read_temperatures, device)
  end

end
