defmodule Fern.CLI do
  def main(args) do
    Logger.remove_backend(:console)
    case args do
      [cmd | args] ->
        case cmd do
          x when x in ["c", "compile"] ->
            case args do
              [src_path] ->
                src_path
                |> Fern.AST.parse()
                |> Fern.C.compile()
              _ ->
                raise "compile expects one source path"
            end
          _ ->
            raise "unknown command"
        end
      [] ->
        raise "what do you want me to do?"
    end
  end
end
