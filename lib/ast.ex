defmodule Fern.AST do
  defp fnewfromstr(str) do
    {:str, str}
  end

  defp fnewfrompath(path) do
    f = File.open!(path, [:binary, :read, {:read_ahead, 4194304}])
    {:file, f}
  end

  defp freadline("\n" <> rst, out), do: {out, rst}
  defp freadline("", out), do: {out, ""}
  defp freadline(<<cur>> <> rst, out), do: freadline(rst, out <> <<cur>>)
  defp freadline({:str, str}) do
    case freadline(str, "") do
      {"", ""} ->
        {:eof, {:str, ""}}
      {s, str} ->
        {s, {:str, str}}
    end
  end
  defp freadline({:file, f}) do
    {IO.binread(f, :line), {:file, f}}
  end

  def parse(str) do
    str
    |> fnewfromstr()
    |> build(1, [], nil, [], %{}, 0)
    |> elem(0)
  end

  def parsefile(path) do
    f = fnewfrompath(path)
    b4 = DateTime.utc_now()
    {out, cnt} = build(f, 1, [], nil, [], %{}, 0)
    af = DateTime.utc_now()
    diff = DateTime.diff(af, b4, :microsecond)
    IO.puts :stderr, "finished parsing: #{cnt} cycles in #{Float.round(diff / 1000, 2)}ms (#{Float.round(cnt * 1_000_000 / diff, 2)}cps)"
    IO.inspect out, limit: :infinity, printable_limit: :infinity
    out
  end

  # no unicode support
  # operator precedence:
  # >>
  # <-
  # |
  # ~ ^
  # &
  # b|
  # b~ b^
  # b&
  # = /=
  # < <= > >=
  # b< b>
  # + -
  # * / % (everything up to here is same as with c)
  # @
  # @> .

  # decent preprocessing is done here to make life easier later on
  # - strip comments
  # - remove lines without data
  # in the middle of reading
  defp read_unit(f, height, cnt, out) do
    {line, f} = freadline(f)
    case line do
      str when str != :eof ->
        str
        |> extract_usable()
        |> case do
          "" ->
            read_unit(f, height, cnt + 1, out)
          str ->
            str
            |> final_height(height)
            |> case do
              x when x > 0 ->
                read_unit(f, x, cnt + 1, out <> "\n" <> str)
              0 ->
                {String.trim(out <> "\n" <> str), cnt + 1, f}
            end
        end
    end
  end

  # before reading actual data
  defp read_unit(f) do
    {line, f} = freadline(f)
    case line do
      :eof ->
        nil
      str ->
        str = extract_usable(str)
        str
        |> final_height(0)
        |> case do
          x when x > 0 ->
            read_unit(f, x, 1, str)
          0 ->
            {str, 1, f}
        end
    end
  end

  defp extract_usable(str) do
    str
    |> String.split("--", parts: 2)
    |> case do
      [str, _] ->
        str
      [str] ->
        str
    end
    |> String.trim()
  end

  defp final_height("", height), do: height
  defp final_height("(" <> rst, height), do: final_height(rst, height + 1)
  defp final_height("{" <> rst, height), do: final_height(rst, height + 1)
  defp final_height("}" <> rst, height), do: final_height(rst, height - 1)
  defp final_height(")" <> rst, height), do: final_height(rst, height - 1)
  defp final_height(<<_>> <> rst, height), do: final_height(rst, height)

  # kinda like string searching algo but without repeating first character
  #                 unsafe to ignore height
  defp get_part("", _, partial, _, out), do: String.trim(out <> partial)
  defp get_part(<<cur>> <> rst, height, partial, xs, out) when cur in [?(, ?{], do: get_part(rst, height + 1, "", xs, out <> partial <> <<cur>>)
  defp get_part(<<cur>> <> rst, height, partial, xs, out) when cur in [?), ?}], do: get_part(rst, height - 1, partial, xs, out <> <<cur>>)
  defp get_part(<<cur>> <> rst, height, partial, xs, out) when height > 0, do: get_part(rst, height, partial, xs, out <> <<cur>>)
  # newline for compat later
  defp get_part(<<cur>> <> rst, 0, partial, xs, out) when cur in [?\ , ?\n] do
    case xs do
      [_ | _] ->
        if byte_size(partial) > 1 && String.trim_leading(partial) in xs do
          {String.trim(out), String.trim_leading(partial), rst}
        else
          get_part(rst, 0, <<cur>>, xs, out <> partial)
        end
      _ ->
        if byte_size(partial) > 1 do
          ptrim = String.trim_leading(partial)
          case Regex.run(xs, ptrim) do
            [^ptrim | _] ->
              {String.trim(out), ptrim, rst}
            _ ->
              get_part(rst, 0, <<cur>>, xs, out <> partial)
          end
        else
          get_part(rst, 0, <<cur>>, xs, out <> partial)
        end
    end
  end
  defp get_part(<<cur>> <> rst, 0, partial, xs, out), do: get_part(rst, 0, partial <> <<cur>>, xs, out)
  defp get_part(str, [_ | _] = xs), do: get_part(str, 0, "", xs, "")
  defp get_part(str, xs), do: get_part(str, 0, "", xs, "")

  defp get_unariable("", _, out), do: out
  defp get_unariable(<<cur>> <> rst, height, out) when cur in [?(, ?{], do: get_unariable(rst, height + 1, out <> <<cur>>)
  defp get_unariable(<<cur>> <> rst, height, out) when cur in [?), ?}], do: get_unariable(rst, height - 1, out <> <<cur>>)
  defp get_unariable(<<cur>> <> rst, height, out) when height > 0, do: get_unariable(rst, height, out <> <<cur>>)
  defp get_unariable(<<cur>> <> rst, 0, out) when cur in [?\ , ?\n] do
    out
    |> String.trim()
    |> case do
      "" ->
        out
      out ->
        {out, rst}
    end
  end
  defp get_unariable(<<cur>> <> rst, 0, out), do: get_unariable(rst, 0, out <> <<cur>>)
  defp get_unariable(str), do: get_unariable(String.trim_leading(str), 0, "")

  # like get_term but splitter is " >> " or newline
  # we have guarantee of no empty line
  defp get_statement("", _, partial, out), do: String.trim(out <> partial)
  defp get_statement(<<cur>> <> rst, height, partial, out) when cur in [?(, ?{], do: get_statement(rst, height + 1, "", out <> partial <> <<cur>>)
  defp get_statement(<<cur>> <> rst, height, partial, out) when cur in [?), ?}], do: get_statement(rst, height - 1, partial, out <> <<cur>>)
  defp get_statement(<<cur>> <> rst, height, partial, out) when height > 0, do: get_statement(rst, height, partial, out <> <<cur>>)
  defp get_statement("\n" <> rst, 0, partial, out), do: {String.trim(out <> partial), rst}
  # redundant now :<
  defp get_statement(" " <> rst, 0, partial, out) do
    if byte_size(partial) > 1 && String.trim_leading(partial) == ">>" do
      {String.trim(out), rst}
    else
      get_statement(rst, 0, " ", out <> partial)
    end
  end
  defp get_statement(<<cur>> <> rst, 0, partial, out), do: get_statement(rst, 0, partial <> <<cur>>, out)
  defp get_statement(str), do: get_statement(str, 0, "", "")

  # ending condition
  # 1. hit end of string
  # 2. realize -> for the 2nd time
  defp get_case("", _, statement, partial, _, out), do: String.trim(out <> statement <> partial)
  defp get_case(<<cur>> <> rst, height, statement, partial, cnt, out) when cur in [?(, ?{], do: get_case(rst, height + 1, statement <> partial <> <<cur>>, "", cnt, out)
  defp get_case(<<cur>> <> rst, height, statement, partial, cnt, out) when cur in [?), ?}], do: get_case(rst, height - 1, statement <> <<cur>>, partial, cnt, out)
  defp get_case(<<cur>> <> rst, height, statement, partial, cnt, out) when height > 0, do: get_case(rst, height, statement <> <<cur>>, partial, cnt, out)
  defp get_case(" " <> rst, 0, statement, partial, cnt, out) do
    cond do
      byte_size(partial) > 1 && String.trim_leading(partial) == ">>" ->
        # so that means >> parsing in get_statement is redundant...
        get_case(rst, 0, "", "\n", cnt, out <> statement)
      byte_size(partial) > 1 && String.trim_leading(partial) == "->" ->
        case cnt do
          1 ->
            {String.trim(out), statement <> partial <> "\n" <> rst}
          _ ->
            get_case(rst, 0, "", " ", cnt + 1, out <> statement <> partial)
        end
      true ->
        get_case(rst, 0, statement <> partial, " ", cnt, out)
    end
  end
  defp get_case("\n" <> rst, 0, statement, partial, cnt, out) do
    cond do
      byte_size(partial) > 1 && String.trim_leading(partial) == "->" ->
        case cnt do
          1 ->
            {String.trim(out), statement <> partial <> "\n" <> rst}
          _ ->
            get_case(rst, 0, "", "\n", cnt + 1, out <> statement <> partial)
        end
      true ->
        get_case(rst, 0, "", "\n", cnt, out <> statement <> partial)
    end
  end
  defp get_case(<<cur>> <> rst, 0, statement, partial, cnt, out), do: get_case(rst, 0, statement, partial <> <<cur>>, cnt, out)
  defp get_case(str), do: get_case(str, 0, "", "", 0, "")

  defp get_sec("", _, _, out), do: String.trim(out)
  defp get_sec(<<cur>> <> rst, height, xs, out) when cur in [?(, ?{], do: get_sec(rst, height + 1, xs, out <> <<cur>>)
  defp get_sec(<<cur>> <> rst, height, xs, out) when cur in [?), ?}], do: get_sec(rst, height - 1, xs, out <> <<cur>>)
  defp get_sec(<<cur>> <> rst, height, xs, out) when height > 0, do: get_sec(rst, height, xs, out <> <<cur>>)
  defp get_sec(<<cur>> <> rst, 0, xs, out) do
    if cur in xs do
      {String.trim(out), cur, rst}
    else
      get_sec(rst, 0, xs, out <> <<cur>>)
    end
  end
  defp get_sec(str, xs), do: get_sec(str, 0, xs, "")

  def tokenize_type(str) do
    str
    |> to_charlist()
    |> Enum.sort()
    |> case do
      '8u' -> {:type, :unsigned, 8}
      '4u' -> {:type, :unsigned, 4}
      '2u' -> {:type, :unsigned, 2}
      '1u' -> {:type, :unsigned, 1}
       'u' -> {:type, :unsigned, 1}
      '8s' -> {:type,   :signed, 8}
      '4s' -> {:type,   :signed, 4}
      '2s' -> {:type,   :signed, 2}
      '1s' -> {:type,   :signed, 1}
       's' -> {:type,   :signed, 1}
      '8'  -> {:type, :unsigned, 8}
      '4'  -> {:type, :unsigned, 4}
      '2'  -> {:type, :unsigned, 2}
      '1'  -> {:type, :unsigned, 1}
       ''  -> {:type, :unsigned, 8}
      '8f' -> {:type, :float, 8}
      '4f' -> {:type, :float, 4}
      '2f' -> {:type, :float, 2}
      '1f' -> {:type, :float, 1}
       'f' -> {:type, :float, 1}
    end
  end

  defp tk_mdr("*", a, b), do: {:mult, a, b}
  defp tk_mdr("/", a, b), do: {:div, a, b}
  defp tk_mdr("%", a, b), do: {:rem, a, b}

  defp tk_addsub("+", a, b), do: {:add, a, b}
  defp tk_addsub("-", a, b), do: {:sub, a, b}

  defp tk_bshift("b<", a, b), do: {:bshiftl, a, b}
  defp tk_bshift("b>", a, b), do: {:bshiftr, a, b}

  defp tk_ineq("<", a, b), do: {:lt, a, b}
  defp tk_ineq("<=", a, b), do: {:lte, a, b}
  defp tk_ineq(">", a, b), do: {:gt, a, b}
  defp tk_ineq(">=", a, b), do: {:gte, a, b}

  defp tk_eqneq("=", a, b), do: {:eq, a, b}
  defp tk_eqneq("/=", a, b), do: {:neq, a, b}

  defp tk_biffxor("b~", a, b), do: {:biff, a, b}
  defp tk_biffxor("b^", a, b), do: {:bxor, a, b}

  defp tk_iffxor("~", a, b), do: {:iff, a, b}
  defp tk_iffxor("^", a, b), do: {:xor, a, b}

  defp apply_extract({:apply, b, c}), do: {b, c}
  defp apply_extract(x), do: {x, []}

  # just to extract that nasty cnt + 1
  defp next(f, line, stack, ret, out, kv, cnt), do: build(f, line, stack, ret, out, kv, cnt + 1)

  defp build(f, line, stack, ret, out, kv, cnt) do
    case stack do
      [] ->
        if ret do
          next(f, line, stack, nil, [ret | out], kv, cnt)
        else
          f
          |> read_unit()
          |> case do
            nil ->
              {Enum.reverse(out), cnt + 1}
            {"", nlines, f} ->
              next(f, line + nlines, stack, ret, out, kv, cnt)
            {str, nlines, f} ->
              next(f, line + nlines, [{:assign, str, []}], ret, out, kv, cnt)
          end
        end
      [action | _] ->
        case action do
          {:const, _} = ret ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:var, ""} ->
            next(f, line, tl(stack), :null, out, kv, cnt)
          {:var, x} = ret ->
            # case String.split(x, ".") do
            #   [var, type] ->
            #     next(f, line, tl(stack), {:get, tokenize_type(type), {:var, var}, {:const, "0"}}, out, kv, cnt)
            #   [_] ->
            #     next(f, line, tl(stack), ret, out, kv, cnt)
            # end
            case String.split(x, "(", parts: 2) do
              [name, dat] ->
                ")" <> dat = String.reverse(dat)
                dat = String.reverse(dat)
                # about to return a value; what will it be?
                {f, line, stack, ret, out, kv, cnt} = apply(Macros, String.to_atom(name), [dat, f, line, stack, ret, out, kv, cnt])
                next(f, line, stack, ret, out, kv, cnt)
              [_] ->
                next(f, line, tl(stack), ret, out, kv, cnt)
            end
          {:body, [], xs} ->
            next(f, line, tl(stack), {:body, (xs)}, out, kv, cnt)
          {:body, [st | sts], xs} ->
            if ret do
              next(f, line, [{:body, sts, [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:assign, st, []} | stack], ret, out, kv, cnt)
            end
          {:body, str, xs} ->
            [_ | stack] = stack
            case get_statement(str) do
              {x, rst} ->
                next(f, line, [{:body, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                next(f, line, [{:body, [x | xs], []} | stack], ret, out, kv, cnt)
            end
          {:head, [], [a, b]} ->
            xs = case a do
              {:apply, x, xs} ->
                [x | xs]
              {:var, _} = x ->
                [x]
              :null ->
                :null
            end
            next(f, line, tl(stack), {:head, xs, b}, out, kv, cnt)
          {:head, [a], xs} ->
            if ret do
              next(f, line, [{:head, [], [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:apply, a, []} | stack], ret, out, kv, cnt)
            end
          {:head, [b, a], xs} ->
            if ret do
              next(f, line, [{:head, [a], [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:or, b, []} | stack], ret, out, kv, cnt)
            end
          {:head, str, xs} ->
            [_ | stack] = stack
            case get_sec(str, ';') do
              {x, _, rst} ->
                next(f, line, [{:head, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:apply, x, []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:head, xs, []} | stack], ret, out, kv, cnt)
                end
            end
          {:case, [], [a, b]} ->
            a = case a do
              {:apply, x, xs} ->
                {:head, [x | xs], :null}
              {:var, _} = x ->
                {:head, [x], :null}
              x ->
                x
            end
            next(f, line, tl(stack), {:case, a, b}, out, kv, cnt)
          {:case, [a], xs} ->
            if ret do
              next(f, line, [{:case, [], [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:head, a, []} | stack], ret, out, kv, cnt)
            end
          {:case, [b, a], xs} ->
            if ret do
              next(f, line, [{:case, [a], [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:body, b, []} | stack], ret, out, kv, cnt)
            end
          {:case, str, xs} ->
            [_ | stack] = stack
            case get_part(str, ["->"]) do
              {x, _, rst} ->
                next(f, line, [{:case, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:body, x, []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:case, xs, []} | stack], ret, out, kv, cnt)
                end
            end
          {:func, [], xs} ->
            next(f, line, tl(stack), {:func, xs}, out, kv, cnt)
          {:func, [c | cs], xs} ->
            if ret do
              next(f, line, [{:func, cs, [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:case, c, []} | stack], ret, out, kv, cnt)
            end
          {:func, str, xs} ->
            [_ | stack] = stack
            #             just to be safe
            case get_case(" " <> str) do
              {x, rst} ->
                next(f, line, [{:func, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                next(f, line, [{:func, [x | xs], []} | stack], ret, out, kv, cnt)
            end
          {:term, x} -> # can materialize to a variety of things
            [_ | stack] = stack
            case x do
              "(" <> x -> # subexpression
                ")" <> x = String.reverse(x)
                x = String.reverse(x)
                # for now no nested assignments
                next(f, line, [{:or, x, []} | stack], ret, out, kv, cnt)
              "{" <> x -> # function
                "}" <> x = String.reverse(x)
                x = String.reverse(x)
                next(f, line, [{:func, String.trim(x), []} | stack], ret, out, kv, cnt)
              <<n>> <> _ = x when ?0 <= n and n <= ?9 or n == ?. -> # constant
                # make sure to wrap the constant later (compile step)
                next(f, line, [{:const, x} | stack], ret, out, kv, cnt)
              x -> # variable
                next(f, line, [{:var, x} | stack], ret, out, kv, cnt)
            end
          {:unary, [], [x], ops} ->
            next(f, line, [{:unary, [], x, ops} | tl(stack)], ret, out, kv, cnt)
          {:unary, [], x, []} ->
            next(f, line, tl(stack), x, out, kv, cnt)
          {:unary, [], x, [op | ops]} ->
            x = case op do
              # rationale on why this does abs():
              # - constants in C that start with 0 are interpereted in octals
              # - this completely differs from math as it would not be otherwise useful
              # - same goes here: + does nothing in math; + does something here (abs)
              # we'll continue to call it pos in case we need to change it later
              # JUST KIDDING TOO LAZY TO IMP ABS
              "+" ->
                {:pos, x}
              "-" ->
                {:neg, x}
              "/" ->
                {:not, x}
              "b/" ->
                {:bnot, x}
              "&" ->
                {:addy, x}
              "^" ->
                {:ver, x}
              "." <> type ->
                {:get, tokenize_type(type), x, {:const, "0"}}
            end
            next(f, line, [{:unary, [], x, ops} | tl(stack)], ret, out, kv, cnt)
          {:unary, [term], [], ops} ->
            if ret do
              next(f, line, [{:unary, [], [ret], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:term, term} | stack], ret, out, kv, cnt)
            end
          {:unary, str, ops} ->
            [_ | stack] = stack
            case str do
              "+" <> x ->
                next(f, line, [{:unary, x, ["+" | ops]} | stack], ret, out, kv, cnt)
              "-" <> x ->
                next(f, line, [{:unary, x, ["-" | ops]} | stack], ret, out, kv, cnt)
              "/" <> x ->
                next(f, line, [{:unary, x, ["/" | ops]} | stack], ret, out, kv, cnt)
              "b/" <> x ->
                next(f, line, [{:unary, x, ["b/" | ops]} | stack], ret, out, kv, cnt)
              "&" <> x ->
                next(f, line, [{:unary, x, ["&" | ops]} | stack], ret, out, kv, cnt)
              "^" <> x ->
                next(f, line, [{:unary, x, ["^" | ops]} | stack], ret, out, kv, cnt)
              "$$" <> x ->
                raise "line #{line}: unknown macro \"#{x}\""
              x ->
                x
                |> String.reverse()
                |> String.split(".", parts: 2)
                |> case do
                  [type, xx] ->
                    if !(String.contains?(type, "(") || String.contains?(type, ")") || String.contains?(type, "{") || String.contains?(type, "}")) do
                      next(f, line, [{:unary, [String.reverse(xx)], [], ["." <> type | ops]} | stack], ret, out, kv, cnt)
                    else
                      case ops do
                        [] ->
                          next(f, line, [{:term, x} | stack], ret, out, kv, cnt)
                        _ ->
                          next(f, line, [{:unary, [x], [], ops} | stack], ret, out, kv, cnt)
                      end
                    end
                  [_] ->
                    case ops do
                      [] ->
                        next(f, line, [{:term, x} | stack], ret, out, kv, cnt)
                      _ ->
                        next(f, line, [{:unary, [x], [], ops} | stack], ret, out, kv, cnt)
                    end
                end
            end
          {:apply, [], xs} ->
            [x | xs] = (xs)
            next(f, line, tl(stack), {:apply, x, List.delete(xs, :null)}, out, kv, cnt)
          {:apply, [unariable | unariables], xs} ->
            if ret do
              next(f, line, [{:apply, unariables, [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:unary, unariable, []} | stack], ret, out, kv, cnt)
            end
          {:apply, str, xs} ->
            [_ | stack] = stack
            case get_unariable(str) do
              {x, rst} ->
                next(f, line, [{:apply, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:unary, x, []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:apply, xs, []} | stack], ret, out, kv, cnt)
                end
            end
          {:setpipe, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:setpipe, [], ret, xs, ops} ->
            [op | ops] = ops
            case op do
              "@>" <> type ->
                type = tokenize_type(type)
                [{:apply, b, [c]} | xs] = xs
                next(f, line, [{:setpipe, [], {:set, type, ret, b, c}, xs, ops} | tl(stack)], ret, out, kv, cnt)
              ".." ->
                [b | xs] = xs
                {b, c} = apply_extract(b)
                next(f, line, [{:setpipe, [], {:apply, b, c ++ [ret]}, xs, ops} | tl(stack)], ret, out, kv, cnt)
              "." ->
                [b | xs] = xs
                {b, c} = apply_extract(b)
                next(f, line, [{:setpipe, [], {:apply, b, [ret | c]}, xs, ops} | tl(stack)], ret, out, kv, cnt)
              "." <> idx ->
                idx = String.to_integer(idx)
                [b | xs] = xs
                {b, c} = apply_extract(b)
                next(f, line, [{:setpipe, [], {:apply, b, List.insert_at(c, idx, ret)}, xs, ops} | tl(stack)], ret, out, kv, cnt)
            end
          {:setpipe, [], xs, ops} ->
            [op | ops] = Enum.reverse(ops)
            case op do
              "@>" <> type ->
                type = tokenize_type(type)
                [a, {:apply, b, [c]} | xs] = xs
                next(f, line, [{:setpipe, [], {:set, type, a, b, c}, xs, ops} | tl(stack)], ret, out, kv, cnt)
              ".." ->
                [a, b | xs] = xs
                {b, c} = apply_extract(b)
                next(f, line, [{:setpipe, [], {:apply, b, c ++ [a]}, xs, ops} | tl(stack)], ret, out, kv, cnt)
              "." ->
                [a, b | xs] = xs
                {b, c} = apply_extract(b)
                next(f, line, [{:setpipe, [], {:apply, b, [a | c]}, xs, ops} | tl(stack)], ret, out, kv, cnt)
              "." <> idx ->
                idx = String.to_integer(idx)
                [a, b | xs] = xs
                {b, c} = apply_extract(b)
                next(f, line, [{:setpipe, [], {:apply, b, List.insert_at(c, idx, a)}, xs, ops} | tl(stack)], ret, out, kv, cnt)
            end
          {:setpipe, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:setpipe, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:apply, part, []} | stack], ret, out, kv, cnt)
            end
          {:setpipe, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ~r/@>.*|\..*/) do
              {x, op, rst} ->
                next(f, line, [{:setpipe, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:apply, x, []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:setpipe, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:get, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:get, [], ret, xs, ops} ->
            ["@" <> type | ops] = ops
            type = tokenize_type(type)
            [b | xs] = xs
            # yes I'm aware you shouldn't be putting ret here ---------------------VVV
            next(f, line, [{:get, [], {:get, type, ret, b}, xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:get, [], xs, ops} ->
            ["@" <> type | ops] = Enum.reverse(ops)
            type = tokenize_type(type)
            [a, b | xs] = (xs)
            next(f, line, [{:get, [], {:get, type, a, b}, xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:get, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:get, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:setpipe, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:get, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ~r/@([^>].*|)/) do
              {x, op, rst} ->
                next(f, line, [{:get, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:setpipe, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:get, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:mdr, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:mdr, [], ret, [x | xs], [op | ops]} ->
            next(f, line, [{:mdr, [], tk_mdr(op, ret, x), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:mdr, [], xs, ops} ->
            [op | ops] = Enum.reverse(ops)
            [a, b | xs] = (xs)
            next(f, line, [{:mdr, [], tk_mdr(op, a, b), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:mdr, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:mdr, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:get, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:mdr, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ["*", "/", "%"]) do
              {x, op, rst} ->
                next(f, line, [{:mdr, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:get, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:mdr, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:addsub, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:addsub, [], ret, [x | xs], [op | ops]} ->
            next(f, line, [{:addsub, [], tk_addsub(op, ret, x), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:addsub, [], xs, ops} ->
            [op | ops] = Enum.reverse(ops)
            [a, b | xs] = (xs)
            next(f, line, [{:addsub, [], tk_addsub(op, a, b), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:addsub, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:addsub, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:mdr, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:addsub, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ["+", "-"]) do
              {x, op, rst} ->
                next(f, line, [{:addsub, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:mdr, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:addsub, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:bshift, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:bshift, [], ret, [x | xs], [op | ops]} ->
            next(f, line, [{:bshift, [], tk_bshift(op, ret, x), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:bshift, [], xs, ops} ->
            [op | ops] = Enum.reverse(ops)
            [a, b | xs] = (xs)
            next(f, line, [{:bshift, [], tk_bshift(op, a, b), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:bshift, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:bshift, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:addsub, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:bshift, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ["b<", "b>"]) do
              {x, op, rst} ->
                next(f, line, [{:bshift, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:addsub, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:bshift, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:ineq, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:ineq, [], ret, [x | xs], [op | ops]} ->
            next(f, line, [{:ineq, [], tk_ineq(op, ret, x), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:ineq, [], xs, ops} ->
            [op | ops] = Enum.reverse(ops)
            [a, b | xs] = (xs)
            next(f, line, [{:ineq, [], tk_ineq(op, a, b), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:ineq, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:ineq, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:bshift, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:ineq, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ["<", "<=", ">", ">="]) do
              {x, op, rst} ->
                next(f, line, [{:ineq, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:bshift, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:ineq, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:eqneq, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:eqneq, [], ret, [x | xs], [op | ops]} ->
            next(f, line, [{:eqneq, [], tk_eqneq(op, ret, x), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:eqneq, [], xs, ops} ->
            [op | ops] = Enum.reverse(ops)
            [a, b | xs] = (xs)
            next(f, line, [{:eqneq, [], tk_eqneq(op, a, b), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:eqneq, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:eqneq, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:ineq, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:eqneq, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ["=", "/="]) do
              {x, op, rst} ->
                next(f, line, [{:eqneq, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:ineq, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:eqneq, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:band, [], ret, []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:band, [], ret, [x | xs]} ->
            next(f, line, [{:band, [], {:band, ret, x}, xs} | tl(stack)], ret, out, kv, cnt)
          {:band, [], xs} ->
            [a, b | xs] = (xs)
            next(f, line, [{:band, [], {:band, a, b}, xs} | tl(stack)], ret, out, kv, cnt)
          {:band, [part | parts], xs} ->
            if ret do
              next(f, line, [{:band, parts, [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:eqneq, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:band, str, xs} ->
            [_ | stack] = stack
            case get_part(str, ["b&"]) do
              {x, _, rst} ->
                next(f, line, [{:band, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:eqneq, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:band, xs, []} | stack], ret, out, kv, cnt)
                end
            end
          {:biffxor, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:biffxor, [], ret, [x | xs], [op | ops]} ->
            next(f, line, [{:biffxor, [], tk_biffxor(op, ret, x), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:biffxor, [], xs, ops} ->
            [op | ops] = Enum.reverse(ops)
            [a, b | xs] = (xs)
            next(f, line, [{:biffxor, [], tk_biffxor(op, a, b), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:biffxor, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:biffxor, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:band, part, []} | stack], ret, out, kv, cnt)
            end
          {:biffxor, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ["b~", "b^"]) do
              {x, op, rst} ->
                next(f, line, [{:biffxor, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:band, x, []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:biffxor, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:bor, [], ret, []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:bor, [], ret, [x | xs]} ->
            next(f, line, [{:bor, [], {:bor, ret, x}, xs} | tl(stack)], ret, out, kv, cnt)
          {:bor, [], xs} ->
            [a, b | xs] = (xs)
            next(f, line, [{:bor, [], {:bor, a, b}, xs} | tl(stack)], ret, out, kv, cnt)
          {:bor, [part | parts], xs} ->
            if ret do
              next(f, line, [{:bor, parts, [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:biffxor, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:bor, str, xs} ->
            [_ | stack] = stack
            case get_part(str, ["b|"]) do
              {x, _, rst} ->
                next(f, line, [{:bor, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:biffxor, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:bor, xs, []} | stack], ret, out, kv, cnt)
                end
            end
          {:and, [], ret, []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:and, [], ret, [x | xs]} ->
            next(f, line, [{:and, [], {:and, ret, x}, xs} | tl(stack)], ret, out, kv, cnt)
          {:and, [], xs} ->
            [a, b | xs] = (xs)
            next(f, line, [{:and, [], {:and, a, b}, xs} | tl(stack)], ret, out, kv, cnt)
          {:and, [part | parts], xs} ->
            if ret do
              next(f, line, [{:and, parts, [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:bor, part, []} | stack], ret, out, kv, cnt)
            end
          {:and, str, xs} ->
            [_ | stack] = stack
            case get_part(str, ["&"]) do
              {x, _, rst} ->
                next(f, line, [{:and, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:bor, x, []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:and, xs, []} | stack], ret, out, kv, cnt)
                end
            end
          {:iffxor, [], ret, [], []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:iffxor, [], ret, [x | xs], [op | ops]} ->
            next(f, line, [{:iffxor, [], tk_iffxor(op, ret, x), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:iffxor, [], xs, ops} ->
            [op | ops] = Enum.reverse(ops)
            [a, b | xs] = (xs)
            next(f, line, [{:iffxor, [], tk_iffxor(op, a, b), xs, ops} | tl(stack)], ret, out, kv, cnt)
          {:iffxor, [part | parts], xs, ops} ->
            if ret do
              next(f, line, [{:iffxor, parts, [ret | xs], ops} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:and, part, []} | stack], ret, out, kv, cnt)
            end
          {:iffxor, str, xs, ops} ->
            [_ | stack] = stack
            case get_part(str, ["~", "^"]) do
              {x, op, rst} ->
                next(f, line, [{:iffxor, rst, [x | xs], [op | ops]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:and, x, []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:iffxor, xs, [], ops} | stack], ret, out, kv, cnt)
                end
            end
          {:or, [], ret, []} ->
            next(f, line, tl(stack), ret, out, kv, cnt)
          {:or, [], ret, [x | xs]} ->
            next(f, line, [{:or, [], {:or, ret, x}, xs} | tl(stack)], ret, out, kv, cnt)
          {:or, [], xs} ->
            [a, b | xs] = (xs)
            next(f, line, [{:or, [], {:or, a, b}, xs} | tl(stack)], ret, out, kv, cnt)
          {:or, [part | parts], xs} ->
            if ret do
              next(f, line, [{:or, parts, [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:iffxor, part, [], []} | stack], ret, out, kv, cnt)
            end
          {:or, str, xs} ->
            [_ | stack] = stack
            case get_part(str, ["|"]) do
              {x, _, rst} ->
                next(f, line, [{:or, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:iffxor, x, [], []} | stack], ret, out, kv, cnt)
                  xs ->
                    next(f, line, [{:or, xs, []} | stack], ret, out, kv, cnt)
                end
            end
          {:assign, [], [a, b]} ->
            next(f, line, tl(stack), {:assign, a, b}, out, kv, cnt)
          {:assign, [part | parts], xs} ->
            if ret do
              next(f, line, [{:assign, parts, [ret | xs]} | tl(stack)], nil, out, kv, cnt)
            else
              next(f, line, [{:or, part, []} | stack], ret, out, kv, cnt)
            end
          {:assign, str, xs} ->
            [_ | stack] = stack
            case get_part(str, ["<-"]) do
              {x, _, rst} ->
                next(f, line, [{:assign, rst, [x | xs]} | stack], ret, out, kv, cnt)
              x ->
                case [x | xs] do
                  [x] ->
                    next(f, line, [{:or, x, []} | stack], ret, out, kv, cnt)
                  [_, _] = xs ->
                    next(f, line, [{:assign, xs, []} | stack], ret, out, kv, cnt)
                  _ ->
                    raise "line #{line}: assigns are not chainable"
                end
            end
        end
    end
  end
end
