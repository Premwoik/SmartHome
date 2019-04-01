# SweetHome

from(t in DB.Task, where: t.id == 2) |> DB.Repo.update_all(set: [execution_time: ~T[18:00:00]])
