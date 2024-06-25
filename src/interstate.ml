open! Core


module City = String
module Highway = struct 
  include String
  let default = ""
end

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
  end (* used to generate the City List *)

  module Connection = struct
    module T = struct
      type t = Highway.t * (City.t * City.t) [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let rec generate_pairs (highway:Highway.t) (cities:City.t list) = 
      match cities with 
      | h :: t -> let list = List.map t ~f:(fun city -> (highway, (h, city))) in
                  List.append list (generate_pairs highway (List.tl_exn cities))
      | _ -> []
    ;;

    let of_string city_lists =
      List.concat_map city_lists ~f:(fun (highway, cities) -> generate_pairs highway cities)
    ;;

  end (* used to generate the pairs needs go be given a String s that has the following format: Highway, City, City *)


  type t = Connection.Set.t [@@deriving sexp_of]
    (* each Network.t has a SET of City Lists (One city list per highway) and a SET of city pairs *)

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
    Connection.Set.of_list (Connection.of_string cities)
    (*City_List.Set.of_list cities*)
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
module G = Graph.Imperative.Graph.ConcreteLabeled (City) (Highway)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes (_v,e,_v2) = [ `Dir `None ;`Label e ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

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
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (highway, (city1, city2)) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge_e graph (city1,highway,city2));
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
