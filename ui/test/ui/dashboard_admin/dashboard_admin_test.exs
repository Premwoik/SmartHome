defmodule Ui.DashboardAdminTest do
  use Ui.DataCase

  alias Ui.DashboardAdmin

  describe "dashboards" do
    alias Ui.DashboardAdmin.Dashboard

    @valid_attrs %{
      description: "some description",
      name: "some name",
      number: 42,
      title: "some title"
    }
    @update_attrs %{
      description: "some updated description",
      name: "some updated name",
      number: 43,
      title: "some updated title"
    }
    @invalid_attrs %{description: nil, name: nil, number: nil, title: nil}

    def dashboard_fixture(attrs \\ %{}) do
      {:ok, dashboard} =
        attrs
        |> Enum.into(@valid_attrs)
        |> DashboardAdmin.create_dashboard()

      dashboard
    end

    test "list_dashboards/0 returns all dashboards" do
      dashboard = dashboard_fixture()
      assert DashboardAdmin.list_dashboards() == [dashboard]
    end

    test "get_dashboard!/1 returns the dashboard with given id" do
      dashboard = dashboard_fixture()
      assert DashboardAdmin.get_dashboard!(dashboard.id) == dashboard
    end

    test "create_dashboard/1 with valid data creates a dashboard" do
      assert {:ok, %Dashboard{} = dashboard} = DashboardAdmin.create_dashboard(@valid_attrs)
      assert dashboard.description == "some description"
      assert dashboard.name == "some name"
      assert dashboard.number == 42
      assert dashboard.title == "some title"
    end

    test "create_dashboard/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = DashboardAdmin.create_dashboard(@invalid_attrs)
    end

    test "update_dashboard/2 with valid data updates the dashboard" do
      dashboard = dashboard_fixture()

      assert {:ok, %Dashboard{} = dashboard} =
               DashboardAdmin.update_dashboard(dashboard, @update_attrs)

      assert dashboard.description == "some updated description"
      assert dashboard.name == "some updated name"
      assert dashboard.number == 43
      assert dashboard.title == "some updated title"
    end

    test "update_dashboard/2 with invalid data returns error changeset" do
      dashboard = dashboard_fixture()

      assert {:error, %Ecto.Changeset{}} =
               DashboardAdmin.update_dashboard(dashboard, @invalid_attrs)

      assert dashboard == DashboardAdmin.get_dashboard!(dashboard.id)
    end

    test "delete_dashboard/1 deletes the dashboard" do
      dashboard = dashboard_fixture()
      assert {:ok, %Dashboard{}} = DashboardAdmin.delete_dashboard(dashboard)
      assert_raise Ecto.NoResultsError, fn -> DashboardAdmin.get_dashboard!(dashboard.id) end
    end

    test "change_dashboard/1 returns a dashboard changeset" do
      dashboard = dashboard_fixture()
      assert %Ecto.Changeset{} = DashboardAdmin.change_dashboard(dashboard)
    end
  end
end
