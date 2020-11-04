defmodule Core.Device.Client do
  @moduledoc false

  @type device_t :: %DB.Device{}
  @type msg_t :: list(integer)
  @type resp_t :: :ok | {:error, any} | {:ok, list(integer)}

  @callback send_msg(device :: device_t, msg :: msg_t) :: resp_t
  @callback send_with_resp(device :: device_t, msg :: msg_t) :: resp_t

end
