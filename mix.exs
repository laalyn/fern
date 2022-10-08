defmodule Fern.MixProject do
  use Mix.Project

  def project do
    [
      app: :fern,
      version: "0.1.0",
      elixir: "~> 1.12",
      escript: [main_module: Fern.CLI, strip_beams: true],
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
