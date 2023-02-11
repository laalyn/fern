defmodule Fern.Shell do
  def start() do
    loop()
  end

  def loop() do
    k = IO.gets ""
    [r] = Fern.AST.parse(k)
    IO.inspect r
    loop()
  end
end
