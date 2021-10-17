defmodule Core.DeviceController do
  @moduledoc false

  import Core.Device, only: [do_: 2]

  def read_inputs(device) do
    do_(:read_inputs, device)
  end

  def read_outputs(device) do
    do_(:read_outputs, device)
  end
end
