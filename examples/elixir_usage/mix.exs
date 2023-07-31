defmodule ElixirUsage.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir_usage,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {ElixirUsage.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:surreal, path: "../../"}
    ]
  end
end
