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
                |> Fern.AST.parsefile()
                |> Fern.C.compile()
              _ ->
                raise "compile expects one source path"
            end
          _ ->
            raise "unknown command"
        end
      [] ->
        Fern.Shell.start()
    end
  end
end
