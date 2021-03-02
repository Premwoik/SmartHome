defmodule Core.Device.DefaultTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import DB

  @device Core.Device.Default
  @client_mock Core.Device.ClientMock

  doctest Core

  import Mox

  setup :verify_on_exit!

  test "invoke set_output" do
    mock(
      [200, 200, 0, 1, 1, 128, 0, 0, 0, 18, 200],
      [200, 200, 0, 100, 0, 0, 0, 0, 0, 14, 200],
      true
    )

    assert @device.set_outputs(device, [25, 12], true) == :ok
  end

  test "invoke set_output - wrong checksum" do
    mock([200, 200, 0, 1, 1, 0, 0, 0, 0, 0, 200], [200, 200, 0, 100, 0, 0, 0, 0, 0, 7, 200], true)

    assert @device.set_outputs(device, [], true) == {:error, "invalid_control_sum"}
  end

  test "invoke set_output - wrong response code" do
    mock(
      [200, 200, 0, 1, 1, 0, 0, 0, 0, 0, 200],
      [200, 200, 0, 101, 0, 0, 0, 0, 0, 14, 200],
      true
    )

    assert @device.set_outputs(device, [], true) == {:error, "wrong response code"}
  end

  test "invoke set_output - fail send" do
    mock([200, 200, 0, 1, 1, 0, 0, 0, 0, 0, 200], {:error, "test"}, false)

    assert @device.set_outputs(device, [], true) == {:error, "test"}
  end

  #  test "invoke set_output - timeout response" do
  #    @client_mock
  #    |> expect(:send_with_resp, fn d, m -> :ok end)
  #
  #    assert @device.set_outputs(device, [], true) == false
  #  end

  def device() do
    %DB.Device{name: "Core.Device.Default"}
  end

  def mock(expected_input, output, send) when send do
    fn d, m ->
      assert expected_input == m
      send(self(), output)
      :ok
    end
    |> prepare_mock()
  end

  def mock(expected_input, output, _) do
    fn d, m ->
      assert expected_input == m
      output
    end
    |> prepare_mock()
  end

  defp prepare_mock(send_with_resp_fn, send_fn \\ nil) do
    @client_mock
    |> expect(:send_with_resp, send_with_resp_fn)

    #    |> expect(:send_msg, send_fn)
  end
end
