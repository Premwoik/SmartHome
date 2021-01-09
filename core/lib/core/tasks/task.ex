defmodule Core.Tasks.Task do
  @moduledoc """

    ## task
    | id | type | active | action_id | device | start_date | end_date | frequency | limit

    ## task_type
    | id | name | module |

    ## task_status
     - inactive
     - waiting
     - running
     - ended



    Use situations:
    - read_device_inputs
    - cyclical execute action
    -

  """
  alias DB.Task

  @callback execute(task :: %Task{}, state :: map) :: {:ok, state :: map} | :error
  @callback init_state() :: map

  #  @callback reload() :: :succ | :fail
end
