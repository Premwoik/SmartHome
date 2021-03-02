import Config

#config :configurator, server: :"rpi@192.168.2.100"
config :configurator, server: :"server@192.168.2.105"

name = System.get_env("USER") <> "@configurator"
Node.start(String.to_atom(name))
Node.set_cookie(:"123")

import_config "../../db/config/config.exs"
