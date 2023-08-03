defmodule Example.Users do
  alias :surreal, as: Surreal

  @conn Example.Database
  @table "users"

  def add(name, age) do
    Surreal.create(@conn, "#{@table}:#{name}", %{"age" => age, "verified" => false})
  end

  def get(name) do
    Surreal.select(@conn, "#{@table}:#{name}")
  end

  def get_all() do
    Surreal.select(@conn, @table)
  end

  def verify(name) do
    Surreal.merge(@conn, "#{@table}:#{name}", %{"verified" => true})
  end

  def remove(name) do
    Surreal.delete(@conn, "#{@table}:#{name}")
  end
end
