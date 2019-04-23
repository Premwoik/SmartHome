defmodule Core.Device.Default.ProtocolTest do
  @moduledoc false
  use ExUnit.Case

  alias Core.Device.Default.Protocol
  import DB

  doctest Core

  import Mox

  test "invoke encode" do
    assert Protocol.encode(0, 100, [0,0,0,0,0]) == [200, 200, 0, 100, 0, 0, 0, 0, 0, 14, 200]
    assert Protocol.encode(2, 251, [0,0,0,0,0]) == [200, 200, 2, 251, 0, 0, 0, 0, 0, 36, 200]
    assert Protocol.encode(0, 100, [50, 3, 2, 90, 100]) == [200, 200, 0, 100, 50, 3, 2, 90, 100, 49, 200]
  end


  test "invoke decode" do
    assert Protocol.decode([200, 200, 0, 100, 0, 0, 0, 0, 0, 14, 200]) == {:ok, [0, 100, 0, 0, 0, 0, 0]}
    assert Protocol.decode([201, 200, 0, 100, 0, 0, 0, 0, 0, 14, 200]) == {:error, "wrong_header"}
    assert Protocol.decode([200, 201, 0, 100, 0, 0, 0, 0, 0, 14, 200]) == {:error, "wrong_header"}
    assert Protocol.decode([200, 200, 0, 100, 0, 0, 0, 0, 0, 14, 202]) == {:error, "wrong_footer"}
    assert Protocol.decode([200, 200, 0, 100, 0, 0, 0, 0, 0, 15, 200]) == {:error, "invalid_control_sum"}
  end
end
