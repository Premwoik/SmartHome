defmodule Core.Controllers.WattMeterController do
  @moduledoc false

  def read(device) do
    Core.Device.do_(:read_watts, device)
  end
end
