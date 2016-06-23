open Yojson.Basic;

let packageJsonPath = Sys.argv.(1);

let deps =
  from_file packageJsonPath |>
    Util.member "jengaboot" |>
    Util.to_option (fun a => a |> Util.member "ppxFlag" |> Util.to_string_option);

switch deps {
  | None
  | Some None => print_endline ""
  | Some (Some x) => print_endline x
};
