defmodule Example.Application do
  use Application

  alias :surreal, as: Surreal

  def start(_type, _args) do
    children = [
      {Surreal, {'surrealdb://root:root@localhost:8000/test/test', Example.Database, %{}}}
    ]

    opts = [name: Example.Supervisor, strategy: :one_for_one]
    Supervisor.start_link(children, opts)
  end
end
