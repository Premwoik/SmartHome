defmodule Ui.LightAdminTest do
  use Ui.DataCase

  alias Ui.LightAdmin

  describe "lights" do
    alias Ui.LightAdmin.Light

    @valid_attrs %{device_id: 42, dimmer_id: 42, name: "some name", number: 42, state: true}
    @update_attrs %{device_id: 43, dimmer_id: 43, name: "some updated name", number: 43, state: false}
    @invalid_attrs %{device_id: nil, dimmer_id: nil, name: nil, number: nil, state: nil}

    def light_fixture(attrs \\ %{}) do
      {:ok, light} =
        attrs
        |> Enum.into(@valid_attrs)
        |> LightAdmin.create_light()

      light
    end

    test "list_lights/0 returns all lights" do
      light = light_fixture()
      assert LightAdmin.list_lights() == [light]
    end

    test "get_light!/1 returns the light with given id" do
      light = light_fixture()
      assert LightAdmin.get_light!(light.id) == light
    end

    test "create_light/1 with valid data creates a light" do
      assert {:ok, %Light{} = light} = LightAdmin.create_light(@valid_attrs)
      assert light.device_id == 42
      assert light.dimmer_id == 42
      assert light.name == "some name"
      assert light.number == 42
      assert light.state == true
    end

    test "create_light/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = LightAdmin.create_light(@invalid_attrs)
    end

    test "update_light/2 with valid data updates the light" do
      light = light_fixture()
      assert {:ok, %Light{} = light} = LightAdmin.update_light(light, @update_attrs)
      assert light.device_id == 43
      assert light.dimmer_id == 43
      assert light.name == "some updated name"
      assert light.number == 43
      assert light.state == false
    end

    test "update_light/2 with invalid data returns error changeset" do
      light = light_fixture()
      assert {:error, %Ecto.Changeset{}} = LightAdmin.update_light(light, @invalid_attrs)
      assert light == LightAdmin.get_light!(light.id)
    end

    test "delete_light/1 deletes the light" do
      light = light_fixture()
      assert {:ok, %Light{}} = LightAdmin.delete_light(light)
      assert_raise Ecto.NoResultsError, fn -> LightAdmin.get_light!(light.id) end
    end

    test "change_light/1 returns a light changeset" do
      light = light_fixture()
      assert %Ecto.Changeset{} = LightAdmin.change_light(light)
    end
  end
end
