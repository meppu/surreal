# Example Elixir Mix Project

This exemplary Mix project demonstrates the usage of the SurrealDB Erlang library in Elixir.

## Setup

### SurrealDB

1. Install SurrealDB (if not already installed) by following the official installation instructions: [SurrealDB Installation Guide](https://surrealdb.com/docs/installation).

2. Start the SurrealDB server:

   ```bash
   surrealdb start --user root --pass root memory --log debug
   ```

### Project

1. Install Elixir (if not already installed) by following the official installation instructions: [Elixir Installation Guide](https://elixir-lang.org/install.html).

2. Clone the repository and navigate to the project directory:

   ```bash
   git clone https://github.com/meppu/surreal.git
   cd ./surreal/examples/elixir_mix
   ```

3. Fetch and compile the dependencies:

   ```bash
   mix deps.get
   mix deps.compile
   ```

4. Ensure to update connection URI in `lib/application.ex` for your environment.

## Running the Example

To execute the example, start an interactive Elixir shell (iex):

**NOTE**: You may need to alter `iex` to `iex.bat` on Windows!

```bash
iex -S mix
```

Now you can interact with the `Example.Users` module:

```elixir
iex(1)> Example.Users.add("meppu", 16)
{:ok, %{"age" => 16, "id" => "users:meppu", "verified" => false}}
iex(2)> Example.Users.get("meppu")
{:ok, %{"age" => 16, "id" => "users:meppu", "verified" => false}}
iex(3)> Example.Users.verify("meppu")
{:ok, %{"age" => 16, "id" => "users:meppu", "verified" => true}}
iex(4)> Example.Users.add("tuhana", 18)
{:ok, %{"age" => 18, "id" => "users:tuhana", "verified" => false}}
iex(5)> Example.Users.get_all()
{:ok,
 [
   %{"age" => 16, "id" => "users:meppu", "verified" => true},
   %{"age" => 18, "id" => "users:tuhana", "verified" => false}
 ]}
iex(6)> Example.Users.remove("meppu")
{:ok, %{"age" => 16, "id" => "users:meppu", "verified" => true}}
iex(7)> Example.Users.get_all()
{:ok, [%{"age" => 18, "id" => "users:tuhana", "verified" => false}]}
```
