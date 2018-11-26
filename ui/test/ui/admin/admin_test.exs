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

    @valid_attrs %{active: true, end_time: ~T[14:00:00], frequency: 42, function: "some function", params: "some params", port_id: 42, start_time: ~T[14:00:00]}
    @update_attrs %{active: false, end_time: ~T[15:01:01], frequency: 43, function: "some updated function", params: "some updated params", port_id: 43, start_time: ~T[15:01:01]}
    @invalid_attrs %{active: nil, end_time: nil, frequency: nil, function: nil, params: nil, port_id: nil, start_time: nil}

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
end
