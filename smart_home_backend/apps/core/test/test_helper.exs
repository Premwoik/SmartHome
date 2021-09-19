Application.ensure_all_started(:mox)
ExUnit.start()
DB.start(:test)
DB.clear_data()
DB.init_test()

Mox.defmock(Core.Device.ClientMock, for: Core.Device.Client)
Mox.defmock(Core.DeviceMock, for: Core.Device)
Mox.defmock(Core.Actions.ActionMock, for: Core.Actions.Action)
Mox.defmock(Core.Utils.Time.Mock, for: Core.Utils.Time)
Mox.defmock(Core.Utils.DateTime.Mock, for: Core.Utils.DateTime)
Mox.defmock(Core.Tasks.TaskMock, for: Core.Tasks.Task)

# Application.put_env(:core, :calculator, MyApp.CalcMock)
