defmodule Ui.AdminSunblindTest do
  use Ui.DataCase

  alias Ui.AdminSunblind

  describe "sunblinds" do
    alias Ui.AdminSunblind.Sunblind

    @valid_attrs %{direction: "some direction", full_open_time: 42, position: 42, state: "some state", type: "some type"}
    @update_attrs %{direction: "some updated direction", full_open_time: 43, position: 43, state: "some updated state", type: "some updated type"}
    @invalid_attrs %{direction: nil, full_open_time: nil, position: nil, state: nil, type: nil}

    def sunblind_fixture(attrs \\ %{}) do
      {:ok, sunblind} =
        attrs
        |> Enum.into(@valid_attrs)
        |> AdminSunblind.create_sunblind()

      sunblind
    end

    test "list_sunblinds/0 returns all sunblinds" do
      sunblind = sunblind_fixture()
      assert AdminSunblind.list_sunblinds() == [sunblind]
    end

    test "get_sunblind!/1 returns the sunblind with given id" do
      sunblind = sunblind_fixture()
      assert AdminSunblind.get_sunblind!(sunblind.id) == sunblind
    end

    test "create_sunblind/1 with valid data creates a sunblind" do
      assert {:ok, %Sunblind{} = sunblind} = AdminSunblind.create_sunblind(@valid_attrs)
      assert sunblind.direction == "some direction"
      assert sunblind.full_open_time == 42
      assert sunblind.position == 42
      assert sunblind.state == "some state"
      assert sunblind.type == "some type"
    end

    test "create_sunblind/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = AdminSunblind.create_sunblind(@invalid_attrs)
    end

    test "update_sunblind/2 with valid data updates the sunblind" do
      sunblind = sunblind_fixture()
      assert {:ok, %Sunblind{} = sunblind} = AdminSunblind.update_sunblind(sunblind, @update_attrs)
      assert sunblind.direction == "some updated direction"
      assert sunblind.full_open_time == 43
      assert sunblind.position == 43
      assert sunblind.state == "some updated state"
      assert sunblind.type == "some updated type"
    end

    test "update_sunblind/2 with invalid data returns error changeset" do
      sunblind = sunblind_fixture()
      assert {:error, %Ecto.Changeset{}} = AdminSunblind.update_sunblind(sunblind, @invalid_attrs)
      assert sunblind == AdminSunblind.get_sunblind!(sunblind.id)
    end

    test "delete_sunblind/1 deletes the sunblind" do
      sunblind = sunblind_fixture()
      assert {:ok, %Sunblind{}} = AdminSunblind.delete_sunblind(sunblind)
      assert_raise Ecto.NoResultsError, fn -> AdminSunblind.get_sunblind!(sunblind.id) end
    end

    test "change_sunblind/1 returns a sunblind changeset" do
      sunblind = sunblind_fixture()
      assert %Ecto.Changeset{} = AdminSunblind.change_sunblind(sunblind)
    end
  end
end
