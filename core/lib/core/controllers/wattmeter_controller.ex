defmodule Core.Controllers.WattMeterController do
  @moduledoc false

  @callback read_watts(device :: %DB.Device{}) :: any

  def read(device) do
    Core.Device.do_(:read_watts, device)
  end
end
