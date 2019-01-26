defmodule UiWeb.SunblindView do
  use UiWeb, :view
  alias UiWeb.SunblindView

  def render("index.json", %{sunblinds: sunblinds}) do
    %{data: render_many(sunblinds, SunblindView, "sunblind.json")}
  end

  def render("show.json", %{sunblind: sunblind}) do
    %{data: render_one(sunblind, SunblindView, "sunblind.json")}
  end

  def render("sunblind.json", %{sunblind: sunblind}) do
    %{id: sunblind.id,
      name: sunblind.port.name,
      position: sunblind.position,
      type: sunblind.type,
      full_open_time: sunblind.full_open_time,
      direction: sunblind.direction,
      state: sunblind.state
    }
  end

  #def render("show.json", %{dash_sunblind: sunblind}) do
    #%{data: render_one(sunblind, SunblindView, "dash_sunblind.json")}
  #end

  #def render("dash_sunblind.json", %{sunblind: sunblind}) do
    #%{id: sunblind.id,
      #name: sunblind.port.name,
      #state: sunblind.state,
      #sunblind: ""
    #}
  #end

end
