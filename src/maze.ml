open! Core

(* type state = | Open | Wall | End | Start | None (* type direction = | Down
   | Up | Left | Right | None *) *)
let get_direction x_cur y_cur x_last y_last =
  let x_dif = x_last - x_cur in
  let y_dif = y_last - y_cur in
  match x_dif, y_dif with
  | -1, 0 -> "Down"
  | 1, 0 -> "Up"
  | 0, 1 -> "Left"
  | 0, -1 -> "Right"
  | _ -> "" (* should not ever happen *)
;;

let are_neighbors x_cur y_cur x_last y_last =
  let x_dif = x_last - x_cur in
  let y_dif = y_last - y_cur in
  if ((equal x_dif (-1) || equal x_dif 1) && equal y_dif 0)
     || ((equal y_dif (-1) || equal y_dif 1) && equal x_dif 0)
  then true
  else false
;;

module Point = struct
  module T = struct
    type t = (Int.t * Int.t) * String.t [@@deriving compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)
end

let rec dfs maze visited last_visited =
  Hash_set.add visited last_visited;
  let (last_x, last_y), last_state = last_visited in
  if String.equal last_state "End"
  then Some ""
  else (
    let open_neighbors =
      List.filter maze ~f:(fun ((x, y), state) ->
        are_neighbors x y last_x last_y
        && (String.equal state "Open" || String.equal state "End")
        && not (Hash_set.mem visited ((x, y), state)))
    in
    print_s [%message (open_neighbors : ((int * int) * string) list)];
    List.find_map open_neighbors ~f:(fun neighbor ->
      match dfs maze visited neighbor with
      | None -> None
      | Some result ->
        let (x, y), _s = neighbor in
        Some
          (String.append
             (String.append (get_direction x y last_x last_y) " ")
             result)))
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let lines = In_channel.read_lines (File_path.to_string input_file) in
        let chars_and_x =
          List.mapi lines ~f:(fun x s -> String.to_list s, x)
        in
        let maze =
          List.concat_map chars_and_x ~f:(fun (c, x) ->
            List.mapi c ~f:(fun y char ->
              match char with
              | '#' -> (x, y), "Wall"
              | '.' -> (x, y), "Open"
              | 'S' -> (x, y), "Start"
              | 'E' -> (x, y), "End"
              | _ -> failwith "Bad input"))
        in
        print_s [%message (maze : ((int * int) * string) list)];
        let visited = Hash_set.create (module Point) in
        let start =
          List.find_exn maze ~f:(fun (_point, state) ->
            String.equal state "Start")
        in
        match dfs maze visited start with
        | None -> printf " No path found "
        | Some result -> printf !"%s" result]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
