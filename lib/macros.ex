defmodule Macros do
  import Fern.AST, only: [parse: 1]

  defp expr(x) do
    [x] = parse(x)
    x
  end

  # FIXME FOR ALL MACROS
  # - go down to the first function you see and inject the code there

  # TODO strsub
  def str(dat, f, line, stack, _ret, out, kv, cnt) do
    stri = Map.get(kv, :stri, 0)
    kv = Map.put(kv, :stri, stri + 1)
    out = [expr("__str#{stri} <- new #{byte_size(dat) + 1}") | out]
    out = Enum.reduce(Enum.with_index(to_charlist(dat) ++ [0]), out, fn {cur, i}, out ->
      [expr("__str#{stri} <- __str#{stri} @>1 #{i} #{cur}") | out]
    end)
    {f, line, tl(stack), {:var, "__str#{stri}"}, out, kv, cnt}
  end

  # TODO null returns
  def setflag(dat, f, line, stack, _ret, out, kv, cnt) do
    IO.puts "setflag: #{inspect(dat)}"
    kv = Map.put(kv, dat, true)
    {f, line, tl(stack), expr("0"), out, kv, cnt}
  end

  def unsetflag(dat, f, line, stack, _ret, out, kv, cnt) do
    IO.puts "unsetflag: #{inspect(dat)}"
    kv = Map.delete(kv, dat)
    {f, line, tl(stack), expr("0"), out, kv, cnt}
  end

  def if(dat, f, line, stack, _ret, out, kv, cnt) do
    IO.puts "HERE VVV"
    IO.inspect stack
    [flag, code] = String.split(dat, "\n", parts: 2)
    [_, {:body, sts, xs} | stack] = stack
    xs = if Map.get(kv, flag) do
      IO.puts code
      Enum.reverse(parse(code)) ++ xs
    else
      xs
    end
    stack = [{:body, sts, xs} | stack]
    IO.inspect stack
    IO.puts "^^^^^^^^"
    {f, line, stack, expr("0"), out, kv, cnt}
  end
end
