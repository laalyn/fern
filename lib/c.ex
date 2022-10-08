defmodule Fern.C do
  # only two "types"
  # 1. scalars
  # - new (*)
  # - [] @> (*) (*)
  # - [] @ (*)
  # - (*) <binary-op> (*)
  # - (*)<unary-op>
  # - <var> ...; (*) ->
  # 2. boxes
  # - [*] @> () ()
  # - [*] @ ()
  # - del [*]
  # - <var> [*] [*] [*] ...
  # - <var> <- [*]

  defp shift(str, n) do
    w = List.duplicate(?\ , n)
        |> to_string()
    str
    |> String.split("\n")
    |> Enum.map(&(w <> &1))
    |> Enum.join("\n")
  end

  defp ctype({:type, :unsigned, 1}), do: "unsigned char"
  defp ctype({:type, :unsigned, 2}), do: "unsigned short"
  defp ctype({:type, :unsigned, 4}), do: "unsigned int"
  defp ctype({:type, :unsigned, 8}), do: "unsigned long"
  defp ctype({:type, :signed, 1}), do: "signed char"
  defp ctype({:type, :signed, 2}), do: "short"
  defp ctype({:type, :signed, 4}), do: "int"
  defp ctype({:type, :signed, 8}), do: "long"
  defp ctype({:type, :float, 4}), do: "float"
  defp ctype({:type, :float, 8}), do: "double"

  defp type({:type, :unsigned, n}), do: "u#{n}"
  defp type({:type, :signed, n}), do: "s#{n}"
  defp type({:type, :float, n}), do: "f#{n}"

  defp varname(x), do: x |> String.replace(":", "___") |> String.replace("-", "__")

  defp autoinc(0), do: ""
  defp autoinc(n) do
    autoinc = 0..((n - 1) * 2 + 1)//1
              |> Enum.map(&("_#{&1}"))
              |> Enum.join(", ")
    "var " <> autoinc <> ";"
  end

  defp file(ast) do
    {mainc, types, typed, funcs} = main(ast)
    injected = ""
    "#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#{types}
typedef unsigned long var;
#define VAR_MAX ULONG_MAX
#{injected}
void new(var size, var *ptr, var *ver) {
  var new = (var) malloc(8 + size);
  *((var *) new) = 0;
  *ptr = new;
  *ver = 0;
}
#{typed}
var getvar(var ptr, var ver, var ofs) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"getvar version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  return *((var *) (ptr + 8 + ofs));
}
void setvar(var ptr, var ver, var ofs, var val, var *ptro, var *vero) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"setvar version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  if (ver == VAR_MAX) {
    fputs(\"setvar version overflow\\n\", stderr);
    abort();
  }
  (*((var *) ptr))++;
  *((var *) (ptr + 8 + ofs)) = val;
  *ptro = ptr;
  *vero = ver + 1;
}
void getbox(var ptr, var ver, var ofs, var *ptro, var *vero) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"getbox version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  *ptro = *((var *) (ptr + 8 + ofs));
  *vero = *((var *) (ptr + 8 + ofs + sizeof (var)));
}
void setbox(var ptr, var ver, var ofs, var val_ptr, var val_ofs, var *ptro, var *vero) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"setbox version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  if (ver == VAR_MAX) {
    fputs(\"setbox version overflow\\n\", stderr);
    abort();
  }
  (*((var *) ptr))++;
  *((var *) (ptr + 8 + ofs)) = val_ptr;
  *((var *) (ptr + 8 + ofs + sizeof (var))) = val_ofs;
  *ptro = ptr;
  *vero = ver + 1;
}
var cpy_s(var size, var ptr, var ver, var ofs, var scalar, var *ptro, var *vero) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"cpy_s version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  if (ver == VAR_MAX) {
    fputs(\"cpy_s version overflow\\n\", stderr);
    abort();
  }
  (*((var *) ptr))++;
  memcpy((void *) (ptr + 8 + ofs), &scalar, size);
  *ptro = ptr;
  *vero = ver + 1;
}
var cpy_b(var size, var ptr, var ver, var ofs, var box_ptr, var box_ver, var box_ofs, var *ptro, var *vero) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"cpy_b version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  if (*((var *) box_ptr) != box_ver) {
    fprintf(stderr, \"cpy_b box version mismatch %lu %lu\\n\", *((var *) box_ptr), box_ver);
    abort();
  }
  if (ver == VAR_MAX) {
    fputs(\"setvar version overflow\\n\", stderr);
    abort();
  }
  (*((var *) ptr))++;
  memcpy((void *) (ptr + 8 + ofs), (void *) (box_ptr + 8 + box_ofs), size);
  *ptro = ptr;
  *vero = ver + 1;
}
var ptrassert(var ptr, var ver) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"ptrassert version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  return ptr;
}
var _sptr, _spos;
#define STACK_SIZE #{System.get_env("STACK_SIZE")}
void new_s(var size, var *ptr, var *ver) {
  if (_spos + 8 + size > STACK_SIZE) {
    fputs(\"new_s stack overflow\\n\", stderr);
    abort();
  }
  *ptr = _sptr + _spos;
  *((var *) (*ptr)) = 0;
  *ver = 0;
  _spos += 8 + size;
}
#{funcs}
int main(int argc, char **argv) {
  _sptr = (var) malloc(STACK_SIZE);
  _spos = 0;
#{shift(mainc, 2)}
}
"
# max(_smax + 8 + size, min(_smax * 2, ))
  end

  defp main(ast) do
    #                   nobody really cares
    {out, types, bvars, _max_depth, funcs} = body(ast, "()")
    %{} = bvars
    typelist = types
               |> Enum.map(&elem(&1, 0))
    types = typelist
            |> Enum.map(&("typedef #{ctype(&1)} #{type(&1)};"))
            |> Enum.join("\n")
    typed = Enum.map(typelist, fn cur ->
      "#{ctype(cur)} get#{type(cur)}(var ptr, var ver, var ofs) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"get#{type(cur)} version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  return *((#{type(cur)} *) (ptr + 8 + ofs));
}
void set#{type(cur)}(var ptr, var ver, var ofs, #{type(cur)} val, var *ptro, var *vero) {
  if (*((var *) ptr) != ver) {
    fprintf(stderr, \"set#{type(cur)} version mismatch %lu %lu\\n\", *((var *) ptr), ver);
    abort();
  }
  if (ver == VAR_MAX) {
    fputs(\"set#{type(cur)} version overflow\\n\", stderr);
    abort();
  }
  (*((var *) ptr))++;
  *((#{type(cur)} *) (ptr + 8 + ofs)) = val;
  *ptro = ptr;
  *vero = ver + 1;
}"
    end)
    |> Enum.join("\n")

    {out, types, typed, funcs}
  end

  defp body([], _func, out, vars, types, bvars, max_depth, funcs, _arity, dng) do
    vars = (Enum.map(vars, &elem(&1, 0)) -- dng) -- Enum.map(bvars, &elem(&1, 0))
    p = if vars != [] do
      "var #{Enum.join(List.flatten(Enum.map(vars, fn k -> ["#{k}_ptr", "#{k}_ver"] end)), ", ")};\n"
    else
      ""
    end
    # FIXME prolly don't have to forward down the max depth; in general check the fishy depth usage
    {autoinc(max_depth) <> "\n" <> p <> out, types, bvars, max_depth, funcs}
  end
  defp body([cur | rst], func, out, vars, types, bvars, max_depth, funcs, arity, dng) do
    IO.inspect cur
    IO.puts ""
    case cur do
      {:assign, _, _} = x ->
        {var, pre, this_types, this_bvars, this_funcs, this_max_depth} = assign(x, func, vars)
        body(rst, func, out <> String.trim(pre) <> "\n", Map.put(vars, var, true), Map.merge(types, this_types), Map.merge(bvars, this_bvars), max(max_depth, this_max_depth), funcs <> String.trim(this_funcs) <> "\n", arity, dng)
      {:apply, {:var, "$"}, args} when rst == [] and length(args) == arity ->
        {out, types, bvars, max_depth, funcs, defer} = Enum.reduce(Enum.with_index(args), {out, types, bvars, max_depth, funcs, ""}, fn {cur, i}, {out, types, bvars, max_depth, funcs, defer} ->
          # shouldn't need to utilize depth here
          {:box, ptr, ver, pre, this_types, this_bvars, this_funcs, this_max_depth} = expr(cur, func, vars, max_depth)
          {out <> "\n" <> pre, Map.merge(types, this_types), Map.merge(bvars, this_bvars), max(max_depth, this_max_depth), funcs <> "\n" <> this_funcs, defer <> "\n_v#{i}_ptr = #{ptr};\n_v#{i}_ver = #{ver};"}
        end)
        body(rst, func, out <> defer <>"\n", vars, types, bvars, max_depth, funcs, arity, dng)
      x ->
        case expr(x, func, vars) do
          {:nothing, pre, this_types, this_bvars, this_funcs, this_max_depth} ->
            case rst do
              [_ | _] ->
                body(rst, func, out <> String.trim(pre) <> "\n", vars, Map.merge(types, this_types), Map.merge(bvars, this_bvars), max(max_depth, this_max_depth), funcs <> String.trim(this_funcs) <> "\n", arity, dng)
            end
          {:scalar, res, pre, this_types, this_bvars, this_funcs, this_max_depth} ->
            case rst do
              [] ->
                # not the cleanest but whatever
                body(rst, func, out <> String.trim(pre) <> "\nreturn #{res};\n", vars, Map.merge(types, this_types), Map.merge(bvars, this_bvars), max(max_depth, this_max_depth), funcs <> String.trim(this_funcs) <> "\n", arity, dng)
              [_ | _] ->
                body(rst, func, out <> String.trim(pre) <> "\n#{res};\n", vars, Map.merge(types, this_types), Map.merge(bvars, this_bvars), max(max_depth, this_max_depth), funcs <> String.trim(this_funcs) <> "\n", arity, dng)
            end
          {:box, ptr, ver, pre, this_types, this_bvars, this_funcs, this_max_depth} ->
            case rst do
              [] ->
                body(rst, func, out <> String.trim(pre) <> "\n*_ptr = #{ptr};\n*_ver = #{ver};\nbreak;", vars, Map.merge(types, this_types), Map.merge(bvars, this_bvars), max(max_depth, this_max_depth), funcs <> String.trim(this_funcs) <> "\n", arity, dng)
              [_ | _] ->
                body(rst, func, out <> String.trim(pre) <> "\n", vars, Map.merge(types, this_types), Map.merge(bvars, this_bvars), max(max_depth, this_max_depth), funcs <> String.trim(this_funcs) <> "\n", arity, dng)
            end
        end
    end
  end
  defp body(ast, func, vars, arity, dng) do
    body(ast, func, "", vars, %{}, %{}, 0, "", arity, dng)
  end
  # defp body(ast, func, vars), do: body(ast, func, "", vars, %{}, %{}, 0, "", [])
  defp body(ast, func), do: body(ast, func, "", %{}, %{}, %{}, 0, "", nil, [])

  # expr ret
  # - needs func, vars
  # - {:scalar, res, pre, types, bvars, funcs, max_depth}
  # - {:box, ptr, ver, pre, types, bvars, funcs, max_depth}
  #                      ro     ro     val
  defp expr({:const, x}, _func, _vars, depth) do
    x = String.replace(x, "_", "")
    {:scalar, "#{x}", "", %{}, %{}, "", depth}
  end
  defp expr({:var, "#b"}, _func, _vars, depth) do
    {:scalar, "2 * sizeof (var)", "", %{}, %{}, "", depth}
  end
  defp expr({:var, "#" <> type}, _func, _vars, depth) do
    {:scalar, "sizeof (#{type(Fern.AST.tokenize_type(type))})", "", %{Fern.AST.tokenize_type(type) => true}, %{}, "", depth}
  end
  defp expr({:var, x}, _func, vars, depth) do
    x = varname(x)
    if vars[x] do
      {:box, "#{x}_ptr", "#{x}_ver", "", %{}, %{}, "", depth}
    else
      {:box, "#{x}_ptr", "#{x}_ver", "", %{}, %{x => true}, "", depth}
    end
  end
  defp expr({:pos, x}, func, vars, depth) do
    {:scalar, res, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:scalar, "+(#{res})", pre, types, bvars, funcs, depth}
  end
  defp expr({:neg, x}, func, vars, depth) do
    {:scalar, res, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:scalar, "-(#{res})", pre, types, bvars, funcs, depth}
  end
  defp expr({:not, x}, func, vars, depth) do
    {:scalar, res, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:scalar, "!(#{res})", pre, types, bvars, funcs, depth}
  end
  defp expr({:bnot, x}, func, vars, depth) do
    {:scalar, res, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:scalar, "~(#{res})", pre, types, bvars, funcs, depth}
  end
  defp expr({:addy, x}, func, vars, depth) do
    {:box, ptr, _ver, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:scalar, "#{ptr}", pre, types, bvars, funcs, depth}
  end
  defp expr({:ver, x}, func, vars, depth) do
    {:box, _ptr, ver, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:scalar, "#{ver}", pre, types, bvars, funcs, depth}
  end
  defp expr({:apply, {:var, "New"}, [size]}, func, vars, depth) do
    {:scalar, size, pre, types, bvars, funcs, depth} = expr(size, func, vars, depth)
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", pre <> "\nnew(#{size}, &_#{depth * 2}, &_#{depth * 2 + 1});", types, bvars, funcs, depth + 1}
  end
  defp expr({:apply, {:var, "new"}, [size]}, func, vars, depth) do
    {:scalar, size, pre, types, bvars, funcs, depth} = expr(size, func, vars, depth)
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", pre <> "\nnew_s(#{size}, &_#{depth * 2}, &_#{depth * 2 + 1});", types, bvars, funcs, depth + 1}
  end
  defp expr({:apply, {:var, "Return" <> type}, [x]}, func, vars, depth) do
    {:scalar, x, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", pre <> "\nnew(sizeof (#{type(Fern.AST.tokenize_type(type))}), &_#{depth * 2}, &_#{depth * 2 + 1});\n*((#{type(Fern.AST.tokenize_type(type))} *) (_#{depth * 2} + 8)) = #{x};\n", types, bvars, funcs, depth + 1}
  end
  defp expr({:apply, {:var, "return" <> type}, [x]}, func, vars, depth) do
    {:scalar, x, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", pre <> "\nnew_s(sizeof (#{type(Fern.AST.tokenize_type(type))}), &_#{depth * 2}, &_#{depth * 2 + 1});\n*((#{type(Fern.AST.tokenize_type(type))} *) (_#{depth * 2} + 8)) = #{x};\n", types, bvars, funcs, depth + 1}
  end
  defp expr({:apply, {:var, "repkg" <> type}, [box, scalar]}, func, vars, depth) do
    {:box, box_ptr, box_ver, box_pre, box_types, box_bvars, box_funcs, depth} = expr(box, func, vars, depth)
    {:scalar, scalar_res, scalar_pre, scalar_types, scalar_bvars, scalar_funcs, depth} = expr(scalar, func, vars, depth)
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", box_pre <> "\n" <> scalar_pre <> "\nset#{type(Fern.AST.tokenize_type(type))}(#{box_ptr}, #{box_ver}, 0, #{scalar_res}, &_#{depth * 2}, &_#{depth * 2 + 1});", Map.merge(box_types, scalar_types), Map.merge(box_bvars, scalar_bvars), box_funcs <> "\n" <> scalar_funcs, depth + 1}
  end
  defp expr({:apply, {:var, "del"}, [var]}, func, vars, depth) do
    {:box, ptr, _ver, pre, types, bvars, funcs, depth} = expr(var, func, vars, depth)
    {:nothing, pre <> "\nfree((void *) #{ptr});", types, bvars, funcs, depth}
  end
  defp expr({:apply, {:var, ss}, [x]}, func, vars, depth) when ss in ["stackset", "setstack", "ss"] do
    {:scalar, res, pre, types, bvars, funcs, depth} = expr(x, func, vars, depth)
    {:nothing, pre <> "\n_spos = #{res} - _sptr;", types, bvars, funcs, depth}
  end
  defp expr({:apply, {:var, "cpy"}, [size, var, ofs, val | m_vofs]}, func, vars, depth) do
    {:scalar, size_res, size_pre, size_types, size_bvars, size_funcs, depth} = expr(size, func, vars, depth)
    {:box, var_ptr, var_ver, var_pre, var_types, var_bvars, var_funcs, depth} = expr(var, func, vars, depth)
    {:scalar, ofs_res, ofs_pre, ofs_types, ofs_bvars, ofs_funcs, depth} = expr(ofs, func, vars, depth)
    {vofs_res, vofs_pre, vofs_types, vofs_bvars, vofs_funcs, depth} = case m_vofs do
      [vofs] ->
        {:scalar, vofs_res, vofs_pre, vofs_types, vofs_bvars, vofs_funcs, depth} = expr(vofs, func, vars, depth)
        {vofs_res, vofs_pre, vofs_types, vofs_bvars, vofs_funcs, depth}
      [] ->
        {"0", "", %{}, %{}, "", depth}
    end
    case expr(val, func, vars, depth) do
      {:scalar, val_res, val_pre, val_types, val_bvars, val_funcs, depth} when m_vofs == [] ->
        {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", size_pre <> "\n" <> var_pre <> "\n" <> ofs_pre <> "\n" <> val_pre <> "\ncpy_s(#{size_res}, #{var_ptr}, #{var_ver}, #{ofs_res}, #{val_res}, &_#{depth * 2}, &_#{depth * 2 + 1});", Map.merge(Map.merge(Map.merge(size_types, var_types), ofs_types), val_types), Map.merge(Map.merge(Map.merge(size_bvars, var_bvars), ofs_bvars), val_bvars), size_funcs <> "\n" <> var_funcs <> "\n" <> ofs_funcs <> "\n" <> val_funcs, depth + 1}
      {:box, val_ptr, val_ver, val_pre, val_types, val_bvars, val_funcs, depth} ->
        {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", size_pre <> "\n" <> var_pre <> "\n" <> ofs_pre <> "\n" <> val_pre <> "\n" <> vofs_pre <> "\ncpy_b(#{size_res}, #{var_ptr}, #{var_ver}, #{ofs_res}, #{val_ptr}, #{val_ver}, #{vofs_res}, &_#{depth * 2}, &_#{depth * 2 + 1});", Map.merge(Map.merge(Map.merge(Map.merge(size_types, var_types), ofs_types), val_types), vofs_types), Map.merge(Map.merge(Map.merge(Map.merge(size_bvars, var_bvars), ofs_bvars), val_bvars), vofs_bvars), size_funcs <> "\n" <> var_funcs <> "\n" <> ofs_funcs <> "\n" <> val_funcs <> "\n" <> vofs_funcs, depth + 1}
    end
  end
  defp expr({:apply, {:var, "setbox"}, [var, ofs, val]}, func, vars, depth) do
    {:box, var_ptr, var_ver, var_pre, var_types, var_bvars, var_funcs, depth} = expr(var, func, vars, depth)
    {:scalar, ofs_res, ofs_pre, ofs_types, ofs_bvars, ofs_funcs, depth} = expr(ofs, func, vars, depth)
    {:box, val_ptr, val_ver, val_pre, val_types, val_bvars, val_funcs, depth} = expr(val, func, vars, depth)
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", var_pre <> "\n" <> ofs_pre <> "\n" <> val_pre <> "\nsetbox(#{var_ptr}, #{var_ver}, #{ofs_res}, #{val_ptr}, #{val_ver}, &_#{depth * 2}, &_#{depth * 2 + 1});", Map.merge(Map.merge(Map.merge(var_types, ofs_types), val_types), %{}), Map.merge(Map.merge(var_bvars, ofs_bvars), val_bvars), var_funcs <> "\n" <> ofs_funcs <> "\n" <> val_funcs, depth + 1}
  end
  defp expr({:apply, {:var, "getbox"}, [var, ofs]}, func, vars, depth) do
    {:box, var_ptr, var_ver, var_pre, var_types, var_bvars, var_funcs, depth} = expr(var, func, vars, depth)
    {:scalar, ofs_res, ofs_pre, ofs_types, ofs_bvars, ofs_funcs, depth} = expr(ofs, func, vars, depth)
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", var_pre <> "\n" <> ofs_pre <> "\ngetbox(#{var_ptr}, #{var_ver}, #{ofs_res}, &_#{depth * 2}, &_#{depth * 2 + 1});", Map.merge(Map.merge(var_types, ofs_types), %{}), Map.merge(var_bvars, ofs_bvars), var_funcs <> "\n" <> ofs_funcs, depth + 1}
  end
  # FIXME SCAN AND SCANSTR UNSAFE ASFFFF
  defp expr({:apply, {:var, "scan!"}, [_ | _] = xs}, func, vars, depth) do
    fmt = List.duplicate("%lu", length(xs)) |> Enum.join(" ")
    {expr, pre, types, bvars, funcs, depth} = Enum.reduce(xs, {"", "", %{}, %{}, "", depth}, fn cur, {expr, pre, types, bvars, funcs, depth} ->
      {:box, ptr, _ver, this_pre, this_types, this_bvars, this_funcs, this_depth} = expr(cur, func, vars, depth)
      {expr <> ", (unsigned long *) (#{ptr} + 8)", pre <> "\n" <> this_pre, Map.merge(types, this_types), Map.merge(bvars, this_bvars), funcs <> "\n" <> this_funcs, max(depth, this_depth)}
    end)
    {:nothing, pre <> "\nscanf(\"#{fmt}\"#{expr});", types, bvars, funcs, depth}
  end
  defp expr({:apply, {:var, "scanstr!"}, [_ | _] = xs}, func, vars, depth) do
    fmt = List.duplicate("%s", length(xs)) |> Enum.join(" ")
    {expr, pre, types, bvars, funcs, depth} = Enum.reduce(xs, {"", "", %{}, %{}, "", depth}, fn cur, {expr, pre, types, bvars, funcs, depth} ->
      {:box, ptr, _ver, this_pre, this_types, this_bvars, this_funcs, this_depth} = expr(cur, func, vars, depth)
      {expr <> ", (char *) (#{ptr} + 8)", pre <> "\n" <> this_pre, Map.merge(types, this_types), Map.merge(bvars, this_bvars), funcs <> "\n" <> this_funcs, max(depth, this_depth)}
    end)
    {:nothing, pre <> "\nscanf(\"#{fmt}\"#{expr});", types, bvars, funcs, depth}
  end
  defp expr({:apply, {:var, "print"}, [_ | _] = xs}, func, vars, depth) do
    fmt = List.duplicate("%lu", length(xs)) |> Enum.join(" ")
    {expr, pre, types, bvars, funcs, depth} = Enum.reduce(xs, {"", "", %{}, %{}, "", depth}, fn cur, {expr, pre, types, bvars, funcs, depth} ->
      {:scalar, res, this_pre, this_types, this_bvars, this_funcs, this_depth} = expr(cur, func, vars, depth)
      {expr <> ", (unsigned long) #{res}", pre <> "\n" <> this_pre, Map.merge(types, this_types), Map.merge(bvars, this_bvars), funcs <> "\n" <> this_funcs, max(depth, this_depth)}
    end)
    {:nothing, pre <> "\nprintf(\"#{fmt}\\n\"#{expr});", types, bvars, funcs, depth}
  end
  defp expr({:apply, {:var, "printstr"}, [_ | _] = xs}, func, vars, depth) do
    fmt = List.duplicate("%s", length(xs)) |> Enum.join(" ")
    {expr, pre, types, bvars, funcs, depth} = Enum.reduce(xs, {"", "", %{}, %{}, "", depth}, fn cur, {expr, pre, types, bvars, funcs, depth} ->
      {:box, ptr, ver, this_pre, this_types, this_bvars, this_funcs, this_depth} = expr(cur, func, vars, depth)
      {expr <> ", (char *) (ptrassert(#{ptr}, #{ver}) + 8)", pre <> "\n" <> this_pre, Map.merge(types, this_types), Map.merge(bvars, this_bvars), funcs <> "\n" <> this_funcs, max(depth, this_depth)}
    end)
    {:nothing, pre <> "\nprintf(\"#{fmt}\\n\"#{expr});", types, bvars, funcs, depth}
  end
  defp expr({:apply, {:var, "$"}, xs}, func, vars, depth) do
    {args, pre, types, bvars, funcs, depth} = Enum.reduce(xs, {[], [], %{}, %{}, [], depth}, fn x, {args, pre, types, bvars, funcs, depth} ->
      {:box, ptr, ver, this_pre, this_types, this_bvars, this_funcs, depth} = expr(x, func, vars, depth)
      {[ver, ptr | args], [this_pre | pre], Map.merge(types, this_types), Map.merge(bvars, this_bvars), [this_funcs | funcs], depth}
    end)
    args = Enum.join(Enum.reverse(args), ", ")
    pre = Enum.join(Enum.reverse(pre), "\n")
    funcs = Enum.join(Enum.reverse(funcs), "\n")
    {:box, "*_ptr", "*_ver", pre <> "\n#{func}#{length(xs)}(#{args}, _func, _ptr, _ver);", types, bvars, funcs, depth}
  end
  defp expr({:apply, {:var, "Fn"}, [{:func, xs}]}, _, vars, depth) do
    {prepre, pre, types, funcs, bvars, this_depth} = cases(xs, "_#{Enum.random(0..4294967295)}func", :heap, depth)
    depth = max(depth, this_depth)
    IO.puts "FN_FUNC WRITTEN DEPTH AS #{depth * 2}"
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", prepre <> "\n" <> pre, types, Map.drop(bvars, Enum.map(vars, &elem(&1, 0))), funcs, depth + 1}
  end
  defp expr({:apply, var, xs}, func, vars, depth) do
    xs = if xs == [:null] do
      []
    else
      xs
    end
    {:box, var_ptr, var_ver, var_pre, var_types, var_bvars, var_funcs, depth} = expr(var, func, vars, depth)
    {scaf, arity, args, pre, types, bvars, funcs, depth} = Enum.reduce(xs, {[], 0, [], [var_pre], var_types, var_bvars, [var_funcs], depth}, fn x, {scaf, arity, args, pre, types, bvars, funcs, depth} ->
      {:box, ptr, ver, this_pre, this_types, this_bvars, this_funcs, depth} = expr(x, func, vars, depth)
      {["var", "var" | scaf], arity + 1, [ver, ptr | args], [this_pre | pre], Map.merge(types, this_types), Map.merge(bvars, this_bvars), [this_funcs | funcs], depth}
    end)
    scaf = Enum.join(Enum.reverse(["" | scaf]), ", ")
    args = Enum.join(Enum.reverse(["" | args]), ", ")
    pre = Enum.join(Enum.reverse(pre), "\n")
    funcs = Enum.join(Enum.reverse(funcs), "\n")
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", pre <> "\n((void (*)(#{scaf}var, var *, var *)) getvar(#{var_ptr}, #{var_ver}, #{arity * 8}))(#{args}#{var_ptr}, &_#{depth * 2}, &_#{depth * 2 + 1});", types, bvars, funcs, depth + 1}
  end
  defp expr({:set, type, var, ofs, val}, func, vars, depth) do
    {:box, var_ptr, var_ver, var_pre, var_types, var_bvars, var_funcs, depth} = expr(var, func, vars, depth)
    {:scalar, ofs_res, ofs_pre, ofs_types, ofs_bvars, ofs_funcs, depth} = expr(ofs, func, vars, depth)
    {:scalar, val_res, val_pre, val_types, val_bvars, val_funcs, depth} = expr(val, func, vars, depth)
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", var_pre <> "\n" <> ofs_pre <> "\n" <> val_pre <> "\nset#{type(type)}(#{var_ptr}, #{var_ver}, #{ofs_res}, #{val_res}, &_#{depth * 2}, &_#{depth * 2 + 1});", Map.merge(Map.merge(Map.merge(var_types, ofs_types), val_types), %{type => true}), Map.merge(Map.merge(var_bvars, ofs_bvars), val_bvars), var_funcs <> "\n" <> ofs_funcs <> "\n" <> val_funcs, depth + 1}
  end
  defp expr({:get, type, var, ofs}, func, vars, depth) do
    {:box, var_ptr, var_ver, var_pre, var_types, var_bvars, var_funcs, depth} = expr(var, func, vars, depth)
    {:scalar, ofs_res, ofs_pre, ofs_types, ofs_bvars, ofs_funcs, depth} = expr(ofs, func, vars, depth)
    {:scalar, "get#{type(type)}(#{var_ptr}, #{var_ver}, #{ofs_res})", var_pre <> "\n" <> ofs_pre, Map.merge(Map.merge(var_types, ofs_types), %{type => true}), Map.merge(var_bvars, ofs_bvars), var_funcs <> "\n" <> ofs_funcs, depth}
  end
  defp expr({:mult, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) * (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:div, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) / (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:rem, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) % (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:add, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) + (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:sub, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) - (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:bshiftl, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) << (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:bshiftr, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) >> (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:lt, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) < (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:gt, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) > (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:lte, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) <= (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:gte, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) >= (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:eq, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) == (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:neq, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) != (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:band, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) & (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:biff, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "!((#{a_res}) ^ (#{b_res}))", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:bxor, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) ^ (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:bor, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) | (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:and, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) && (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:iff, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "!(#{a_res}) == !(#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:xor, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "!(#{a_res}) != !(#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  defp expr({:or, a, b}, func, vars, depth) do
    {:scalar, a_res, a_pre, a_types, a_bvars, a_funcs, depth} = expr(a, func, vars, depth)
    {:scalar, b_res, b_pre, b_types, b_bvars, b_funcs, depth} = expr(b, func, vars, depth)
    {:scalar, "(#{a_res}) || (#{b_res})", a_pre <> "\n" <> b_pre, Map.merge(a_types, b_types), Map.merge(a_bvars, b_bvars), a_funcs <> "\n" <> b_funcs, depth}
  end
  # look just solve the name problem later
  # force cases for now
  defp expr({:funca_h, xs}, _, vars, depth) do
    {prepre, pre, types, funcs, bvars, this_depth} = cases(xs, "_#{Enum.random(0..4294967295)}func", :heap, depth)
    depth = max(depth, this_depth)
    IO.puts "FUNCA_H WRITTEN DEPTH AS #{depth * 2}"
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", prepre, pre, types, Map.drop(bvars, Enum.map(vars, &elem(&1, 0))), funcs, depth + 1}
  end
  defp expr({:funca, xs}, _, vars, depth) do
    {prepre, pre, types, funcs, bvars, this_depth} = cases(xs, "_#{Enum.random(0..4294967295)}func", :stack, depth)
    depth = max(depth, this_depth)
    IO.puts "FUNCA WRITTEN DEPTH AS #{depth * 2}"
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", prepre, pre, types, Map.drop(bvars, Enum.map(vars, &elem(&1, 0))), funcs, depth + 1}
  end
  defp expr({:func, xs}, _, vars, depth) do
    # - initializer (pre) code
    # - bake table (funcs)
    # - var dependencies
    # - types
    {prepre, pre, types, funcs, bvars, this_depth} = cases(xs, "_#{Enum.random(0..4294967295)}func", :stack, depth)
    depth = max(depth, this_depth)
    # FIXME yes I am aware depth is scuffed
    IO.puts "FUNC WRITTEN DEPTH AS #{depth * 2}"
    {:box, "_#{depth * 2}", "_#{depth * 2 + 1}", prepre <> "\n" <> pre, types, Map.drop(bvars, Enum.map(vars, &elem(&1, 0))), funcs, depth + 1}
  end
  defp expr(ast, func, vars), do: expr(ast, func, vars, 0)

  # shared between arities:
  # - bake table
  # shared between conds:
  # - positional args
  # - arg names
  # keep to urself:
  # - 
  defp cases([], name, out, types, bvars, max_depth, funcs, alloc) do
    max_arity = Enum.max(Enum.map(out, &elem(&1, 0)))
    bvars_og = bvars
    bvars = Enum.map(bvars, &elem(&1, 0))
    out = Enum.map(out, &(&1))
    alloc = case alloc do
      :stack ->
        "new_s"
      :heap ->
        "new"
    end
    IO.puts "why is max_depth #{max_depth} ??"
    prepre = "#{alloc}(#{(max_arity + 1) * 8 + length(bvars) * 16}, &_#{max_depth * 2}, &_#{max_depth * 2 + 1});"
    pre = Enum.join(Enum.map(out, fn {arity, _} ->
      "*((var *) (_#{max_depth * 2} + #{8 + arity * 8})) = (var) &#{name}#{arity};"
    end), "\n") <> "\n" <> Enum.join(Enum.map(Enum.with_index(bvars), fn {x, i} ->
      "*((var *) (_#{max_depth * 2} + #{8 + (max_arity + 1) * 8 + i * 16})) = #{x}_ptr;\n" <>
      "*((var *) (_#{max_depth * 2} + #{8 + (max_arity + 1) * 8 + i * 16 + 8})) = #{x}_ver;\n"
    end), "\n")
    binit = Enum.join(Enum.map(Enum.with_index(bvars), fn {x, i} ->
      "var #{x}_ptr = *((var *) (_func + #{8 + (max_arity + 1) * 8 + i * 16}));\n" <>
      "var #{x}_ver = *((var *) (_func + #{8 + (max_arity + 1) * 8 + i * 16 + 8}));"
    end), "\n")

    funcs = funcs <> "\n" <> Enum.join(Enum.map(out, fn {arity, cs} ->
      args = Enum.join(Enum.map(0..(arity - 1)//1, &("var _v#{&1}_ptr, var _v#{&1}_ver")) ++ [""], ", ")

      {res, _} = Enum.reduce(Enum.reverse(cs), {"", true}, fn {mappings, test, body}, {acc, first?} ->
        mappings = Enum.map(mappings, &(&1))
        st = Enum.map(mappings, fn {a, b} ->
          "#define #{a} #{b}"
        end)
        |> Enum.join("\n")
        fi = Enum.map(mappings, fn {a, _} ->
          "#undef #{a}"
        end)
        |> Enum.join("\n")

        {acc <> "#{st}\n#{(if first?, do: "if", else: "} else if")} (#{test}) {\n#{shift(body, 2)}\n#{fi}\n", false}
      end)

      "void #{name}#{arity}(#{args}var _func, var *_ptr, var *_ver) {\n" <> shift(binit, 2) <> "\n" <>
      "  while (1) {\n" <>
      "#{shift(res <> "} else {\n  fputs(\"no cases matched\\n\", stderr);\n  abort();\n}", 4)}\n" <>
      "  }\n" <>
      "}"
    end), "\n")

    {prepre, pre, types, funcs, bvars_og, max_depth}
  end
  # out format:
  # - %{arity => [{%{"$named_var" => "$positional_var", ...}, test, body}, ...cases]}
  defp cases([{:case, head, {:body, xs}} | rst], name, out, types, bvars, max_depth, funcs, alloc) do
    {slots, test} = if head == :null do
      {[], :null}
    else
      {:head, slots, test} = head
      {slots, test}
    end
    slots = if slots != :null do
      slots
    else
      []
    end
    {mappings, vars, _} = Enum.reduce(slots, {%{}, %{}, 0}, fn {:var, x}, {acc, vars, i} ->
      x = varname(x)
      {acc
      |> Map.put("#{x}_ptr", "_v#{i}_ptr")
      |> Map.put("#{x}_ver", "_v#{i}_ver"), vars |> Map.put(x, true), i + 1}
    end)
    # you will/should never have anything in test_pre
    
    {test_res, test_pre, test_types, test_bvars, test_funcs, test_depth} = if test != :null do
      {:scalar, test_res, test_pre, test_types, test_bvars, test_funcs, test_depth} = expr(test, name, vars)
      {test_res, test_pre, test_types, test_bvars, test_funcs, test_depth}
    else
      {"1", "", %{}, %{}, "", 0}
    end

    "" = String.trim(test_pre)
    arity = length(slots)
    {body_res, body_types, body_bvars, body_depth, body_funcs} = body(xs, name, vars, arity, Enum.map(slots, fn {:var, x} -> varname(x) end))
    out = if out[arity] do
      out
      |> Map.put(arity, [{mappings, test_res, body_res} | Map.get(out, arity)])
    else
      out
      |> Map.put(arity, [{mappings, test_res, body_res}])
    end
    cases(rst, name, out, Map.merge(Map.merge(types, test_types), body_types), Map.merge(Map.merge(bvars, test_bvars), body_bvars), Enum.max([max_depth, test_depth, body_depth]), funcs <> "\n" <> test_funcs <> "\n" <> body_funcs, alloc)
  end
  defp cases(ast, name, alloc, depth), do: cases(ast, name, %{}, %{}, %{}, depth, "", alloc)

  # max_depth could interperet 0 as nothing due to return behavior
  # - {var, pre, types, bvars, max_depth}
  defp assign({:assign, {:var, var}, {:apply, {:var, "Fn"}, [{:func, xs}]}}, func, vars) do
    var = varname(var)
    {:box, ptr, ver, prepre, pre, types, bvars, funcs, max_depth} = expr({:funca_h, xs}, func, vars)
    {var, prepre <> "\n#{var}_ptr = #{ptr};\n#{var}_ver = #{ver};\n" <> pre, types, bvars, funcs, max_depth}
  end
  defp assign({:assign, {:var, var}, {:func, xs}}, func, vars) do
    var = varname(var)
    {:box, ptr, ver, prepre, pre, types, bvars, funcs, max_depth} = expr({:funca, xs}, func, vars)
    {var, prepre <> "\n#{var}_ptr = #{ptr};\n#{var}_ver = #{ver};\n" <> pre, types, bvars, funcs, max_depth}
  end
  defp assign({:assign, {:var, var}, x}, func, vars) do
    var = varname(var)
    {:box, ptr, ver, pre, types, bvars, funcs, max_depth} = expr(x, func, vars)
    {var, pre <> "\n#{var}_ptr = #{ptr};\n#{var}_ver = #{ver};", types, bvars, funcs, max_depth}
  end

  def compile(ast) do
    # file:
    # - needs base code
    # - needs main code
    # - needs func code
    # - needs types used (to gen base lib)
    # - needs max tmpvar idx
    # base:
    # + base code
    # + C source file (final output)
    # main:
    # - needs expr code
    # - 
    # func:
    # - need func
    # - need assign
    # - need expr
    # + all related functions
    # assign:
    # expr:

    # DYN Outerstuff:
    # - types used
    # - tmpvars
    # - bake table in function
    # - function level vars

    out = file(ast)
    File.write!("out.c", out, [:write])
  end

  # body:
end
