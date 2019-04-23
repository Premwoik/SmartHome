defmodule Ui.PortAdminTest do
  use Ui.DataCase

  alias Ui.PortAdmin

  describe "ports" do
    alias Ui.PortAdmin.Port

    @valid_attrs %{device_id: 42, mode: "some mode", name: "some name", number: 42, state: true, timeout: 42, type: "some type"}
    @update_attrs %{device_id: 43, mode: "some updated mode", name: "some updated name", number: 43, state: false, timeout: 43, type: "some updated type"}
    @invalid_attrs %{device_id: nil, mode: nil, name: nil, number: nil, state: nil, timeout: nil, type: nil}

    def port_fixture(attrs \\ %{}) do
      {:ok, port} =
        attrs
        |> Enum.into(@valid_attrs)
        |> PortAdmin.create_port()

      port
    end

    test "list_ports/0 returns all ports" do
      port = port_fixture()
      assert PortAdmin.list_ports() == [port]
    end

    test "get_port!/1 returns the port with given id" do
      port = port_fixture()
      assert PortAdmin.get_port!(port.id) == port
    end

    test "create_port/1 with valid data creates a port" do
      assert {:ok, %Port{} = port} = PortAdmin.create_port(@valid_attrs)
      assert port.device_id == 42
      assert port.mode == "some mode"
      assert port.name == "some name"
      assert port.number == 42
      assert port.state == true
      assert port.timeout == 42
      assert port.type == "some type"
    end

    test "create_port/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = PortAdmin.create_port(@invalid_attrs)
    end

    test "update_port/2 with valid data updates the port" do
      port = port_fixture()
      assert {:ok, %Port{} = port} = PortAdmin.update_port(port, @update_attrs)
      assert port.device_id == 43
      assert port.mode == "some updated mode"
      assert port.name == "some updated name"
      assert port.number == 43
      assert port.state == false
      assert port.timeout == 43
      assert port.type == "some updated type"
    end

    test "update_port/2 with invalid data returns error changeset" do
      port = port_fixture()
      assert {:error, %Ecto.Changeset{}} = PortAdmin.update_port(port, @invalid_attrs)
      assert port == PortAdmin.get_port!(port.id)
    end

    test "delete_port/1 deletes the port" do
      port = port_fixture()
      assert {:ok, %Port{}} = PortAdmin.delete_port(port)
      assert_raise Ecto.NoResultsError, fn -> PortAdmin.get_port!(port.id) end
    end

    test "change_port/1 returns a port changeset" do
      port = port_fixture()
      assert %Ecto.Changeset{} = PortAdmin.change_port(port)
    end
  end
end
