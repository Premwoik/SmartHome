defmodule Core.Device.Client.Http do
  @moduledoc false

  @type res() :: {:ok, struct()} | {:error, String.t()}

  @spec default_response_catch(%HTTPotion.Response{}, (String.t() -> struct)) :: res()
  def default_response_catch(resp, decode_body) do
    case resp do
      %HTTPotion.Response{status_code: 200, body: body} ->
        body = decode_body.(body)
        {:ok, body}

      %HTTPotion.Response{status_code: status_code} ->
        {:error, "Response with wrong code: #{status_code}"}

      %HTTPotion.ErrorResponse{} ->
        {:error, "Can't connect to device"}
    end
  end

  def skip_response(data, swap \\ nil)

  def skip_response(data, nil) do
    with {:ok, _} <- data, do: :ok
  end

  def skip_response(data, swap) do
    with {:ok, _} <- data, do: {:ok, swap}
  end

  def to_atom_map(mp) do
    for {key, val} <- mp, into: %{}, do: {String.to_atom(key), val}
  end
end
