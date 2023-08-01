# Elixir Example Mix Project

This example mix project demonstrates the usage of the SurrealDB erlang library in Elixir.

## Setup

### SurrealDB

1. Install SurrealDB (if not already installed) by following the official installation instructions: [SurrealDB Installation Guide](https://surrealdb.com/docs/installation).

2. Start SurrealDB server:

   ```bash
   # An example!
   surrealdb start --user root --pass root memory --log debug
   ```

### Project

1. Install Elixir (if not already installed) by following the official installation instructions: [Elixir Installation Guide](https://elixir-lang.org/install.html).

2. Clone the repository and navigate to the project directory:

   ```bash
   git clone https://github.com/meppu/surreal.git
   cd ./surreal/examples/elixir_usage
   ```

3. Fetch and compile the dependencies:

   ```bash
   mix deps.get
   mix deps.compile
   ```

4. Make sure to update connection URL in `lib/application.ex` for your environment.

## Running the Example

Make sure to change the SurrealDB URL in `application.ex` for your own one.

To run the example, start an interactive Elixir shell (iex):

**NOTE**: You may need to change `iex` to `iex.bat` on Windows!

```bash
iex -S mix
```

Now you can play with `ElixirUsage.Users` module:

```elixir
iex(1)> ElixirUsage.Users.add("meppu", 16)
{:ok, %{"age" => 16, "id" => "users:meppu", "verified" => false}}
iex(2)> ElixirUsage.Users.get("meppu")
{:ok, %{"age" => 16, "id" => "users:meppu", "verified" => false}}
iex(3)> ElixirUsage.Users.verify("meppu")
{:ok, %{"age" => 16, "id" => "users:meppu", "verified" => true}}
iex(4)> ElixirUsage.Users.add("tuhana", 18)
{:ok, %{"age" => 18, "id" => "users:tuhana", "verified" => false}}
iex(5)> ElixirUsage.Users.get_all()
{:ok,
 [
   %{"age" => 16, "id" => "users:meppu", "verified" => true},
   %{"age" => 18, "id" => "users:tuhana", "verified" => false}
 ]}
iex(6)> ElixirUsage.Users.remove("meppu")
{:ok, %{"age" => 16, "id" => "users:meppu", "verified" => true}}
iex(7)> ElixirUsage.Users.get_all()
{:ok, [%{"age" => 18, "id" => "users:tuhana", "verified" => false}]}
```
