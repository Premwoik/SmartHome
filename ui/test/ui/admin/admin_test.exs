defmodule Ui.AdminTest do
  use Ui.DataCase

  alias Ui.Admin

  describe "users" do
    alias Ui.Admin.User

    @valid_attrs %{age: 42, name: "some name"}
    @update_attrs %{age: 43, name: "some updated name"}
    @invalid_attrs %{age: nil, name: nil}

    def user_fixture(attrs \\ %{}) do
      {:ok, user} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Admin.create_user()

      user
    end

    test "list_users/0 returns all users" do
      user = user_fixture()
      assert Admin.list_users() == [user]
    end

    test "get_user!/1 returns the user with given id" do
      user = user_fixture()
      assert Admin.get_user!(user.id) == user
    end

    test "create_user/1 with valid data creates a user" do
      assert {:ok, %User{} = user} = Admin.create_user(@valid_attrs)
      assert user.age == 42
      assert user.name == "some name"
    end

    test "create_user/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Admin.create_user(@invalid_attrs)
    end

    test "update_user/2 with valid data updates the user" do
      user = user_fixture()
      assert {:ok, %User{} = user} = Admin.update_user(user, @update_attrs)
      assert user.age == 43
      assert user.name == "some updated name"
    end

    test "update_user/2 with invalid data returns error changeset" do
      user = user_fixture()
      assert {:error, %Ecto.Changeset{}} = Admin.update_user(user, @invalid_attrs)
      assert user == Admin.get_user!(user.id)
    end

    test "delete_user/1 deletes the user" do
      user = user_fixture()
      assert {:ok, %User{}} = Admin.delete_user(user)
      assert_raise Ecto.NoResultsError, fn -> Admin.get_user!(user.id) end
    end

    test "change_user/1 returns a user changeset" do
      user = user_fixture()
      assert %Ecto.Changeset{} = Admin.change_user(user)
    end
  end

  describe "keybs" do
    alias Ui.Admin.Keyb

    @valid_attrs %{age: 42, name: "some name"}
    @update_attrs %{age: 43, name: "some updated name"}
    @invalid_attrs %{age: nil, name: nil}

    def keyb_fixture(attrs \\ %{}) do
      {:ok, keyb} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Admin.create_keyb()

      keyb
    end

    test "list_keybs/0 returns all keybs" do
      keyb = keyb_fixture()
      assert Admin.list_keybs() == [keyb]
    end

    test "get_keyb!/1 returns the keyb with given id" do
      keyb = keyb_fixture()
      assert Admin.get_keyb!(keyb.id) == keyb
    end

    test "create_keyb/1 with valid data creates a keyb" do
      assert {:ok, %Keyb{} = keyb} = Admin.create_keyb(@valid_attrs)
      assert keyb.age == 42
      assert keyb.name == "some name"
    end

    test "create_keyb/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Admin.create_keyb(@invalid_attrs)
    end

    test "update_keyb/2 with valid data updates the keyb" do
      keyb = keyb_fixture()
      assert {:ok, %Keyb{} = keyb} = Admin.update_keyb(keyb, @update_attrs)
      assert keyb.age == 43
      assert keyb.name == "some updated name"
    end

    test "update_keyb/2 with invalid data returns error changeset" do
      keyb = keyb_fixture()
      assert {:error, %Ecto.Changeset{}} = Admin.update_keyb(keyb, @invalid_attrs)
      assert keyb == Admin.get_keyb!(keyb.id)
    end

    test "delete_keyb/1 deletes the keyb" do
      keyb = keyb_fixture()
      assert {:ok, %Keyb{}} = Admin.delete_keyb(keyb)
      assert_raise Ecto.NoResultsError, fn -> Admin.get_keyb!(keyb.id) end
    end

    test "change_keyb/1 returns a keyb changeset" do
      keyb = keyb_fixture()
      assert %Ecto.Changeset{} = Admin.change_keyb(keyb)
    end
  end

  describe "actions" do
    alias Ui.Admin.Action

    @valid_attrs %{
      active: true,
      end_time: ~T[14:00:00],
      frequency: 42,
      function: "some function",
      params: "some params",
      port_id: 42,
      start_time: ~T[14:00:00]
    }
    @update_attrs %{
      active: false,
      end_time: ~T[15:01:01],
      frequency: 43,
      function: "some updated function",
      params: "some updated params",
      port_id: 43,
      start_time: ~T[15:01:01]
    }
    @invalid_attrs %{
      active: nil,
      end_time: nil,
      frequency: nil,
      function: nil,
      params: nil,
      port_id: nil,
      start_time: nil
    }

    def action_fixture(attrs \\ %{}) do
      {:ok, action} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Admin.create_action()

      action
    end

    test "list_actions/0 returns all actions" do
      action = action_fixture()
      assert Admin.list_actions() == [action]
    end

    test "get_action!/1 returns the action with given id" do
      action = action_fixture()
      assert Admin.get_action!(action.id) == action
    end

    test "create_action/1 with valid data creates a action" do
      assert {:ok, %Action{} = action} = Admin.create_action(@valid_attrs)
      assert action.active == true
      assert action.end_time == ~T[14:00:00]
      assert action.frequency == 42
      assert action.function == "some function"
      assert action.params == "some params"
      assert action.port_id == 42
      assert action.start_time == ~T[14:00:00]
    end

    test "create_action/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Admin.create_action(@invalid_attrs)
    end

    test "update_action/2 with valid data updates the action" do
      action = action_fixture()
      assert {:ok, %Action{} = action} = Admin.update_action(action, @update_attrs)
      assert action.active == false
      assert action.end_time == ~T[15:01:01]
      assert action.frequency == 43
      assert action.function == "some updated function"
      assert action.params == "some updated params"
      assert action.port_id == 43
      assert action.start_time == ~T[15:01:01]
    end

    test "update_action/2 with invalid data returns error changeset" do
      action = action_fixture()
      assert {:error, %Ecto.Changeset{}} = Admin.update_action(action, @invalid_attrs)
      assert action == Admin.get_action!(action.id)
    end

    test "delete_action/1 deletes the action" do
      action = action_fixture()
      assert {:ok, %Action{}} = Admin.delete_action(action)
      assert_raise Ecto.NoResultsError, fn -> Admin.get_action!(action.id) end
    end

    test "change_action/1 returns a action changeset" do
      action = action_fixture()
      assert %Ecto.Changeset{} = Admin.change_action(action)
    end
  end

  describe "thermometers" do
    alias Ui.Admin.Thermometer

    @valid_attrs %{address: "some address", device_id: 42, name: "some name", ref: 42}
    @update_attrs %{
      address: "some updated address",
      device_id: 43,
      name: "some updated name",
      ref: 43
    }
    @invalid_attrs %{address: nil, device_id: nil, name: nil, ref: nil}

    def thermometer_fixture(attrs \\ %{}) do
      {:ok, thermometer} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Admin.create_thermometer()

      thermometer
    end

    test "list_thermometers/0 returns all thermometers" do
      thermometer = thermometer_fixture()
      assert Admin.list_thermometers() == [thermometer]
    end

    test "get_thermometer!/1 returns the thermometer with given id" do
      thermometer = thermometer_fixture()
      assert Admin.get_thermometer!(thermometer.id) == thermometer
    end

    test "create_thermometer/1 with valid data creates a thermometer" do
      assert {:ok, %Thermometer{} = thermometer} = Admin.create_thermometer(@valid_attrs)
      assert thermometer.address == "some address"
      assert thermometer.device_id == 42
      assert thermometer.name == "some name"
      assert thermometer.ref == 42
    end

    test "create_thermometer/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Admin.create_thermometer(@invalid_attrs)
    end

    test "update_thermometer/2 with valid data updates the thermometer" do
      thermometer = thermometer_fixture()

      assert {:ok, %Thermometer{} = thermometer} =
               Admin.update_thermometer(thermometer, @update_attrs)

      assert thermometer.address == "some updated address"
      assert thermometer.device_id == 43
      assert thermometer.name == "some updated name"
      assert thermometer.ref == 43
    end

    test "update_thermometer/2 with invalid data returns error changeset" do
      thermometer = thermometer_fixture()
      assert {:error, %Ecto.Changeset{}} = Admin.update_thermometer(thermometer, @invalid_attrs)
      assert thermometer == Admin.get_thermometer!(thermometer.id)
    end

    test "delete_thermometer/1 deletes the thermometer" do
      thermometer = thermometer_fixture()
      assert {:ok, %Thermometer{}} = Admin.delete_thermometer(thermometer)
      assert_raise Ecto.NoResultsError, fn -> Admin.get_thermometer!(thermometer.id) end
    end

    test "change_thermometer/1 returns a thermometer changeset" do
      thermometer = thermometer_fixture()
      assert %Ecto.Changeset{} = Admin.change_thermometer(thermometer)
    end
  end

  describe "wattmeters" do
    alias Ui.Admin.EnergyMeter

    @valid_attrs %{address: 42, device: 42, name: "some name", ref: 42}
    @update_attrs %{address: 43, device: 43, name: "some updated name", ref: 43}
    @invalid_attrs %{address: nil, device: nil, name: nil, ref: nil}

    def energy_meter_fixture(attrs \\ %{}) do
      {:ok, energy_meter} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Admin.create_energy_meter()

      energy_meter
    end

    test "list_wattmeters/0 returns all wattmeters" do
      energy_meter = energy_meter_fixture()
      assert Admin.list_wattmeters() == [energy_meter]
    end

    test "get_energy_meter!/1 returns the energy_meter with given id" do
      energy_meter = energy_meter_fixture()
      assert Admin.get_energy_meter!(energy_meter.id) == energy_meter
    end

    test "create_energy_meter/1 with valid data creates a energy_meter" do
      assert {:ok, %EnergyMeter{} = energy_meter} = Admin.create_energy_meter(@valid_attrs)
      assert energy_meter.address == 42
      assert energy_meter.device == 42
      assert energy_meter.name == "some name"
      assert energy_meter.ref == 42
    end

    test "create_energy_meter/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Admin.create_energy_meter(@invalid_attrs)
    end

    test "update_energy_meter/2 with valid data updates the energy_meter" do
      energy_meter = energy_meter_fixture()

      assert {:ok, %EnergyMeter{} = energy_meter} =
               Admin.update_energy_meter(energy_meter, @update_attrs)

      assert energy_meter.address == 43
      assert energy_meter.device == 43
      assert energy_meter.name == "some updated name"
      assert energy_meter.ref == 43
    end

    test "update_energy_meter/2 with invalid data returns error changeset" do
      energy_meter = energy_meter_fixture()
      assert {:error, %Ecto.Changeset{}} = Admin.update_energy_meter(energy_meter, @invalid_attrs)
      assert energy_meter == Admin.get_energy_meter!(energy_meter.id)
    end

    test "delete_energy_meter/1 deletes the energy_meter" do
      energy_meter = energy_meter_fixture()
      assert {:ok, %EnergyMeter{}} = Admin.delete_energy_meter(energy_meter)
      assert_raise Ecto.NoResultsError, fn -> Admin.get_energy_meter!(energy_meter.id) end
    end

    test "change_energy_meter/1 returns a energy_meter changeset" do
      energy_meter = energy_meter_fixture()
      assert %Ecto.Changeset{} = Admin.change_energy_meter(energy_meter)
    end
  end

  describe "rf_buttons" do
    alias Ui.Admin.RfButton

    @valid_attrs %{
      action: "some action",
      key_value: "some key_value",
      mode: "some mode",
      name: "some name",
      port: "some port",
      task: "some task"
    }
    @update_attrs %{
      action: "some updated action",
      key_value: "some updated key_value",
      mode: "some updated mode",
      name: "some updated name",
      port: "some updated port",
      task: "some updated task"
    }
    @invalid_attrs %{action: nil, key_value: nil, mode: nil, name: nil, port: nil, task: nil}

    def rf_button_fixture(attrs \\ %{}) do
      {:ok, rf_button} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Admin.create_rf_button()

      rf_button
    end

    test "list_rf_buttons/0 returns all rf_buttons" do
      rf_button = rf_button_fixture()
      assert Admin.list_rf_buttons() == [rf_button]
    end

    test "get_rf_button!/1 returns the rf_button with given id" do
      rf_button = rf_button_fixture()
      assert Admin.get_rf_button!(rf_button.id) == rf_button
    end

    test "create_rf_button/1 with valid data creates a rf_button" do
      assert {:ok, %RfButton{} = rf_button} = Admin.create_rf_button(@valid_attrs)
      assert rf_button.action == "some action"
      assert rf_button.key_value == "some key_value"
      assert rf_button.mode == "some mode"
      assert rf_button.name == "some name"
      assert rf_button.port == "some port"
      assert rf_button.task == "some task"
    end

    test "create_rf_button/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Admin.create_rf_button(@invalid_attrs)
    end

    test "update_rf_button/2 with valid data updates the rf_button" do
      rf_button = rf_button_fixture()
      assert {:ok, %RfButton{} = rf_button} = Admin.update_rf_button(rf_button, @update_attrs)
      assert rf_button.action == "some updated action"
      assert rf_button.key_value == "some updated key_value"
      assert rf_button.mode == "some updated mode"
      assert rf_button.name == "some updated name"
      assert rf_button.port == "some updated port"
      assert rf_button.task == "some updated task"
    end

    test "update_rf_button/2 with invalid data returns error changeset" do
      rf_button = rf_button_fixture()
      assert {:error, %Ecto.Changeset{}} = Admin.update_rf_button(rf_button, @invalid_attrs)
      assert rf_button == Admin.get_rf_button!(rf_button.id)
    end

    test "delete_rf_button/1 deletes the rf_button" do
      rf_button = rf_button_fixture()
      assert {:ok, %RfButton{}} = Admin.delete_rf_button(rf_button)
      assert_raise Ecto.NoResultsError, fn -> Admin.get_rf_button!(rf_button.id) end
    end

    test "change_rf_button/1 returns a rf_button changeset" do
      rf_button = rf_button_fixture()
      assert %Ecto.Changeset{} = Admin.change_rf_button(rf_button)
    end
  end

  describe "alarm_partitions" do
    alias Ui.Admin.AlarmPartition

    @valid_attrs %{device_id: 42, name: "some name", number: 42, status: 42}
    @update_attrs %{device_id: 43, name: "some updated name", number: 43, status: 43}
    @invalid_attrs %{device_id: nil, name: nil, number: nil, status: nil}

    def alarm_partition_fixture(attrs \\ %{}) do
      {:ok, alarm_partition} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Admin.create_alarm_partition()

      alarm_partition
    end

    test "list_alarm_partitions/0 returns all alarm_partitions" do
      alarm_partition = alarm_partition_fixture()
      assert Admin.list_alarm_partitions() == [alarm_partition]
    end

    test "get_alarm_partition!/1 returns the alarm_partition with given id" do
      alarm_partition = alarm_partition_fixture()
      assert Admin.get_alarm_partition!(alarm_partition.id) == alarm_partition
    end

    test "create_alarm_partition/1 with valid data creates a alarm_partition" do
      assert {:ok, %AlarmPartition{} = alarm_partition} =
               Admin.create_alarm_partition(@valid_attrs)

      assert alarm_partition.device_id == 42
      assert alarm_partition.name == "some name"
      assert alarm_partition.number == 42
      assert alarm_partition.status == 42
    end

    test "create_alarm_partition/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Admin.create_alarm_partition(@invalid_attrs)
    end

    test "update_alarm_partition/2 with valid data updates the alarm_partition" do
      alarm_partition = alarm_partition_fixture()

      assert {:ok, %AlarmPartition{} = alarm_partition} =
               Admin.update_alarm_partition(alarm_partition, @update_attrs)

      assert alarm_partition.device_id == 43
      assert alarm_partition.name == "some updated name"
      assert alarm_partition.number == 43
      assert alarm_partition.status == 43
    end

    test "update_alarm_partition/2 with invalid data returns error changeset" do
      alarm_partition = alarm_partition_fixture()

      assert {:error, %Ecto.Changeset{}} =
               Admin.update_alarm_partition(alarm_partition, @invalid_attrs)

      assert alarm_partition == Admin.get_alarm_partition!(alarm_partition.id)
    end

    test "delete_alarm_partition/1 deletes the alarm_partition" do
      alarm_partition = alarm_partition_fixture()
      assert {:ok, %AlarmPartition{}} = Admin.delete_alarm_partition(alarm_partition)
      assert_raise Ecto.NoResultsError, fn -> Admin.get_alarm_partition!(alarm_partition.id) end
    end

    test "change_alarm_partition/1 returns a alarm_partition changeset" do
      alarm_partition = alarm_partition_fixture()
      assert %Ecto.Changeset{} = Admin.change_alarm_partition(alarm_partition)
    end
  end
end
