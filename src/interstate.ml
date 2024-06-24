open! Core
module City = String
module Highway = String

module Network = struct
  (* We can represent our interstate graph as a set of connections, where a
     connection represents a highway between two cities. *)

  module City_List = struct
    module T = struct
      type t = Highway.t * City.t list [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let of_string s =
      match String.split s ~on:',' with
      | hd :: tl -> Some (Highway.of_string hd, List.map ~f:City.of_string tl)
      | _ -> None
    ;;
  end

  module Connection = struct
    module T = struct
      type t = Highway.t * (City.t * City.t) [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let of_string s =
      match String.split s ~on:',' with
      | [ a; b ] -> Some (City.of_string a, City.of_string b)
      | _ -> None
    ;;
  end

  type t = City_List.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let cities =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
        match City_List.of_string s with
        | Some (a, b) ->
          (* Highways are mutual; a highway between a and b means we should
             also consider the highway between b and a. *)
          [ a, b ]
        | None ->
          printf
            "ERROR: Could not parse line as connection; dropping. %s\n"
            s;
          [])
    in
    City_List.Set.of_list cities
  ;;

end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values
           (whose types have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;
(*
let get_pairs network:Network.t = 
    
;; *)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        ignore (input_file : File_path.t);
        ignore (output_file : File_path.t);
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
