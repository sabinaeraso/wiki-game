open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)


let not_in_name_space s = 
  let option = Wikipedia_namespace.namespace s in
    match option with 
    | None -> true
    | Some _namespace -> false
;;


let get_linked_articles contents : string list =
  let open Soup in 
  parse contents 
  $$ "a[href*=/wiki/]" 
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:(not_in_name_space)
  |> List.dedup_and_sort ~compare:(String.compare)
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)


  module Url = String
  module Title = String 

  

  module Article  = struct
    module T = struct
      type t = Title.t * Url.t [@@deriving compare, sexp, hash]
   end

  include T
  include Comparable.Make (T)
  end

  module G = Graph.Imperative.Graph.Concrete (Title)

  (* We extend our [Graph] structure with the [Dot] API so that we can easily
     render constructed graphs. Documentation about this API can be found here:
     https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
  module Dot = Graph.Graphviz.Dot (struct
      include G
  
      (* These functions can be changed to tweak the appearance of the
         generated graph. Check out the ocamlgraph graphviz API
         (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
         for examples of what values can be set here. *)
      let edge_attributes _ = [ `Dir `None ]
      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
      let vertex_name v = sprintf !"\"%s\"" v
      let default_vertex_attributes _ = []
      let graph_attributes _ = []
    end)
  (* pairs of articles where the first has the second as a link *)

  let correct_url url (how_to_fetch :File_fetcher.How_to_fetch.t) =
    match how_to_fetch with 
    | Local _ -> url 
    | Remote -> if not (String.is_prefix ~prefix: "https://" url) then "https://en.wikipedia.org" ^ url else url 
  ;;

let get_title link how_to_fetch = 
  let article = File_fetcher.fetch_exn how_to_fetch ~resource:(correct_url link how_to_fetch) in
  let open Soup in 
  parse article 
  $$ "title" 
  |> to_list 
  |> fun x-> List.nth_exn x 0
  |> texts 
  |> String.concat
  |> String.split_on_chars ~on: [' ';'-';'(';')']
  |> String.concat
  |> String.chop_suffix_if_exists ~suffix:"Wikipedia"
  |> fun s-> String.append s " - Wikipedia"
;;



let rec dfs how_to_fetch to_visit max_depth= 
  match Queue.dequeue to_visit with 
  | None -> []
  | Some (article,depth1) -> let current_article = (get_title article how_to_fetch,correct_url article how_to_fetch ) in 
                         let linked = get_linked_articles (File_fetcher.fetch_exn how_to_fetch ~resource:(correct_url article how_to_fetch)) in
                         List.iter linked ~f:(fun art -> Queue.enqueue to_visit (art,depth1 + 1));
                         let linked_articles = List.map linked ~f:(fun s -> (get_title s how_to_fetch, correct_url s how_to_fetch)) in
                         let pairs = List.map linked_articles ~f: (fun a -> (current_article,a)) in
                         if depth1 > max_depth (* regardless of what i change this to its still only giving me these articles in my map *)
                          then pairs
                          else ( 
                         List.append pairs (dfs how_to_fetch to_visit max_depth))
;; 

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let to_visit = Queue.create () in
  Queue.enqueue to_visit (origin,0);
  let pairs = dfs how_to_fetch to_visit max_depth in
  let graph = G.create () in
        List.iter pairs ~f:(fun ((title1, _url1), (title2, _url2)) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge graph title1 title2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)


  let rec bfs how_to_fetch network destination visited to_visit = 
    match Queue.dequeue to_visit with 
      | None -> None
      | Some ((_title,url),path) -> 
        if (String.equal (correct_url url how_to_fetch) destination) 
          then (Some path) 
      else (
        let linked = List.filter network ~f:(fun ((_title1,url1),(title2,url2)) -> (String.equal url1 url) && not(Hash_set.mem visited (title2,url2)) ) in
                        List.iter linked ~f:(fun ((_),(title2,url2)) -> Queue.enqueue to_visit ((title2,url2), path @ [title2]) ; Hash_set.add visited (title2,url2) ; ) ; 
                             bfs how_to_fetch network destination visited to_visit )
  ;;

  let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
    let to_visit_graph = Queue.create () in
    Queue.enqueue to_visit_graph (origin,0);
    let network = dfs how_to_fetch to_visit_graph max_depth in
    let origin_article = (get_title origin how_to_fetch, (correct_url origin how_to_fetch)) in 
    let origin_title,_ = origin_article in
    let to_visit = Queue.create() in 
    let visited = Hash_set.create (module Article) in 
    Hash_set.add visited origin_article ;
    Queue.enqueue to_visit (origin_article,[origin_title]) ;
    bfs how_to_fetch network (correct_url destination how_to_fetch) visited to_visit 
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
