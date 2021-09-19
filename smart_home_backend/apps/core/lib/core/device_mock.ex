defmodule Core.DeviceMock do
  
  alias Core.Device.Static.Response
  
  def do_(_function, _args) do
    Response.error(:not_implemented)
  end

  def do_r(_args, _function) do
    Response.error(:not_implemented)
  end
end
