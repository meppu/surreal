defmodule ElixirUsage.Application do
  use Application

  alias :surreal, as: Surreal

  def start(_type, _args) do
    children = [
      {Surreal, {'surrealdb://root:root@localhost:8000/test/test', ElixirUsage.Database, %{}}}
    ]

    opts = [name: ElixirUsage.Supervisor, strategy: :one_for_one]
    Supervisor.start_link(children, opts)
  end
end
