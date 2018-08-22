defmodule DB.Dao do
  @moduledoc false
  alias DB.Watcher
  alias DB.Device
  alias DB.Repo
  alias DB.Action
  alias DB.Dimmer
  alias DB.DimmersLights
  alias DB.Port
  import Ecto.Query

  def get_lights do
    Repo.all from p in Port, where: p.type == "light",
                             select: %{
                               id: p.id,
                               name: p.name,
                               state: p.state
                             }
  end

  def get_watchers do
    Repo.all(Watcher)
    |> Repo.preload([:device])
  end

  def get_devices do
    Repo.all(Device)
  end

  def find_actions() do
    Repo.all from a in Action,
             join: p in Port,
             join: d in Device,
               #    join: aa in ActionArgument,
             where: a.port_id == p.id,
             where: d.id == p.device_id,
             where: a.active == true,
               #    where: a.id == aa.action_id,
             select: {{d.name, p.number}, {a.id, a.function}}
  end

  def find_action(id) do
    Repo.get(Action, id)
    |> Repo.preload(:args)
  end


  def get_port(id),
      do: Repo.get(Port, id)
          |> Repo.preload(:device)
  def get_ports(ids), do: Repo.all from p in Port, where: p.id in ^ids, preload: :device

  def get_ports_by_type(type), do: get_ports_by_type(type, :nil)
  def get_ports_by_type(type, state) do
    case state do
      :nil -> Repo.all from p in Port, where: p.type == ^type
      _ -> Repo.all from p in Port, where: p.type == ^type and p.state == ^state
    end
  end

  def update_port_state!(port, state) do
    uport = Ecto.Changeset.change port, state: state
    Repo.update! uport
  end

  def update_ports_state(ids, state) do
    Repo.update_all (from p in Port, where: p.id in ^ids),
                    set: [
                      state: state
                    ]
  end

  def update_dimmers_fill(ids, fill, direction) do
    Repo.update_all (from d in Dimmer, where: d.id in ^ids),
                    set: [
                      fill: fill,
                      direction: direction
                    ]
  end

  def get_dimmer_part(id) do
    Repo.get Dimmer, id
  end

  def get_dimmer(id) do
    Repo.all from p in Port, where: p.id== ^id, join: d in Dimmer, on: p.id == d.id, select: {p, d}
  end


  def match_lights_to_dimmers(ids) do
    (
      Repo.all from l in DimmersLights, where: l.port_id in ^ids,
                                        join: d in Dimmer,
                                        on: l.dimmer_id == d.id,
                                        where: d.fill == 0,
                                        join: p in Port,
                                        on: p.id == d.id,
                                        select: {p.device_id, p})
    |> Enum.group_by(&(elem(&1, 0)), fn {_, p} -> p end)
  end

  #   return [ids]
  def get_down_dimmers(lightsIds) do
    dimIds = Repo.all from d in DimmersLights, where: d.port_id in ^lightsIds,
                                               join: d2 in Dimmer,
                                               on: d.dimmer_id == d2.id,
                                               where: d2.fill != 0,
                                               distinct: true,
                                               select: d2.id

    (Repo.all from d in DimmersLights, where: d.dimmer_id in ^dimIds,
                                       join: p in Port,
                                       on: p.id == d.port_id,
                                       group_by: [p.state, d.dimmer_id],
                                       select: {d.dimmer_id, p.state})
    |> (Enum.group_by &(elem &1, 0), &(elem &1, 1))
    |> (Enum.filter fn
      {k, [false]} -> true
      _ -> false
    end)
    |> (Enum.map fn {k, _} -> k end)

  end

  def get_device(id) do
    Repo.get Device, id
  end

  def get_device_atom(id) do
    String.to_atom (Repo.get Device, id).name
  end

  def get_devices(ids) do
    Repo.all from p in Device, where: p.id in ^ids
  end

  def get_ports(device_id) do
    Repo.all from p in Port, where: p.device_id == ^device_id
  end


  def get_dimmers() do
    (Repo.all from d in Dimmer,
              join: p in Port,
              on: p.id == d.id,
              select: {p, d})
    |> (Enum.map fn {x, y} -> Map.merge x, (Repo.preload y, :lights) end)
  end

  def get_sunblinds() do
    d = Repo.all from s in Port, where: s.type == "sunblind"
  end
end
