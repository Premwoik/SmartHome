defmodule Ui.DimmerAdminTest do
  use Ui.DataCase

  alias Ui.DimmerAdmin

  describe "dimmers" do
    alias Ui.DimmerAdmin.Dimmer

    @valid_attrs %{direction: 42, fill: 42, group: true, port_id: 42, time: 42}
    @update_attrs %{direction: 43, fill: 43, group: false, port_id: 43, time: 43}
    @invalid_attrs %{direction: nil, fill: nil, group: nil, port_id: nil, time: nil}

    def dimmer_fixture(attrs \\ %{}) do
      {:ok, dimmer} =
        attrs
        |> Enum.into(@valid_attrs)
        |> DimmerAdmin.create_dimmer()

      dimmer
    end

    test "list_dimmers/0 returns all dimmers" do
      dimmer = dimmer_fixture()
      assert DimmerAdmin.list_dimmers() == [dimmer]
    end

    test "get_dimmer!/1 returns the dimmer with given id" do
      dimmer = dimmer_fixture()
      assert DimmerAdmin.get_dimmer!(dimmer.id) == dimmer
    end

    test "create_dimmer/1 with valid data creates a dimmer" do
      assert {:ok, %Dimmer{} = dimmer} = DimmerAdmin.create_dimmer(@valid_attrs)
      assert dimmer.direction == 42
      assert dimmer.fill == 42
      assert dimmer.group == true
      assert dimmer.port_id == 42
      assert dimmer.time == 42
    end

    test "create_dimmer/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = DimmerAdmin.create_dimmer(@invalid_attrs)
    end

    test "update_dimmer/2 with valid data updates the dimmer" do
      dimmer = dimmer_fixture()
      assert {:ok, %Dimmer{} = dimmer} = DimmerAdmin.update_dimmer(dimmer, @update_attrs)
      assert dimmer.direction == 43
      assert dimmer.fill == 43
      assert dimmer.group == false
      assert dimmer.port_id == 43
      assert dimmer.time == 43
    end

    test "update_dimmer/2 with invalid data returns error changeset" do
      dimmer = dimmer_fixture()
      assert {:error, %Ecto.Changeset{}} = DimmerAdmin.update_dimmer(dimmer, @invalid_attrs)
      assert dimmer == DimmerAdmin.get_dimmer!(dimmer.id)
    end

    test "delete_dimmer/1 deletes the dimmer" do
      dimmer = dimmer_fixture()
      assert {:ok, %Dimmer{}} = DimmerAdmin.delete_dimmer(dimmer)
      assert_raise Ecto.NoResultsError, fn -> DimmerAdmin.get_dimmer!(dimmer.id) end
    end

    test "change_dimmer/1 returns a dimmer changeset" do
      dimmer = dimmer_fixture()
      assert %Ecto.Changeset{} = DimmerAdmin.change_dimmer(dimmer)
    end
  end
end
