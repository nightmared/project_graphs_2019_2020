open Zach;;

(* 
TODO answer :
Do you observe a limitation using the NOVER approach on the karat7
graph ? You can focus on a particular vertex to explain.
*)

module EnsembleSommet = Set.Make(Zach.V);;

let nover_score g = 
  Zach.(
  EnsembleSommet.(
  iter_edges_e (fun e ->
    let u = E.src e and 
        v = E.dst e in 
    let neighbours_u = of_list (succ g u) and
        neighbours_v = of_list (succ g v) in
    let neighbours_both = inter neighbours_u neighbours_v and
        neighbours_at_least = diff (union neighbours_u neighbours_v) (of_list [u;v]) in
    let score = 
      if cardinal neighbours_both <> 0 then float_of_int(cardinal neighbours_both)/.float_of_int(cardinal neighbours_at_least)
      else 0.
    in setscore score e
  ) g
  )
  );;


let%test "nover_score g1" = 
  Zach.(
  Samplegraph.(
  nover_score g1;
  fold_edges_e (
    fun arc correcte -> 
      let score = (
        let label, _ = E.label arc in 
        if label = 1 then 2.0/.2.0 else
        if label = 2 then 2.0/.3.0 else
        if label = 3 then 2.0/.2.0 else 
        if label = 4 then 2.0/.3.0 else
        if label = 5 then 2.0/.3.0 else
        if label = 6 then 0.0 else
        if label = 7 then 2.0/.2.0 else
        if label = 8 then 1.0/.2.0 else
        if label = 9 then 1.0/.2.0 else
        if label = 10 then 1.0/.1.0 
        else failwith "arc inconnu"
      ) in
      correcte && (score = getscore arc)
  ) g1 true
  ));;

(* Graph edge list that satisfies the predicate *)
let find_all_edges_predicate predicate g = 
  Zach.fold_edges_e (
    fun e e_list -> 
      if predicate e then e::e_list else e_list
  ) g [];;

let edges_with_smallest_score g = 
  Zach.(
    let min_score = fold_edges_e (
      fun e min_score -> 
      match min_score with 
      | None -> Some (getscore e)
      | Some (min_score) -> 
        if getscore e < min_score 
        then Some (getscore e) 
        else Some (min_score)
    ) g None in

    match min_score with 
    | None ->  []
    | Some min_score -> find_all_edges_predicate (fun e -> getscore e = min_score) g
  );;


let number_of_connected_component g = 
  Zach.(
    Mark.clear g;
    let rec marque_composante g v = 
      begin
        if Mark.get v = 0 then
        begin 
          Mark.set v 1;
          iter_succ (fun v -> 
            let _ = marque_composante g v in ()
          ) g v;
          1
        end
        else 0
      end 
    in
    fold_vertex(fun v nb_composante -> nb_composante + marque_composante g v) g 0
  );;

let rec step_nover g = 
  Zach.(
    let number_of_connected_components_before_removal = number_of_connected_component g in (
    nover_score g;
    List.iter (remove_edge_e g) (edges_with_smallest_score g); 
    let number_of_connected_components_after_removal = number_of_connected_component g in (
    if number_of_connected_components_after_removal = number_of_connected_components_before_removal
    then step_nover g
    else ()
  )));;

let modularity_of_a_community g community = 
Zach.(
  List.fold_left (fun sum i  ->
    sum +. List.fold_left(fun sum_i j ->
      let coeff_ij = 
        (if i = j then 
          0.
        else
          let ratio = 
            float_of_int((in_degree g i) * (in_degree g j)) /. float_of_int(2 * nb_edges g) and 
          a_ij =
            try
              let _ = find_edge g i j in
              1.0
            with Not_found -> 0.0
          in a_ij -. ratio)
      in sum_i +. coeff_ij 
    )  0.0 community
  ) 0.0 community
);;

let sum_float = List.fold_left (+.) 0.0;;

let modularity g l = 
  let communities_modularity = List.map (modularity_of_a_community g) l in
  sum_float communities_modularity
;;


module VertexHashtbl = Hashtbl.Make (Zach.MyGraph.V);;

let list_vertex g = Zach.fold_vertex (fun v l -> v::l) g [];;

(* Mapping of the vertexes from a copy to the original graph 
  g1 : the copy of the graph
  g2 : the original graph
*)
let map_vertex_between_graph g1 g2 = 
  let mapping_vertex_g1_to_g2 = VertexHashtbl.create 0 in

  List.iter (fun vertex_copy ->
    let original_vertex = List.find (fun vertex_original -> 
      Zach.V.label vertex_original = Zach.V.label vertex_copy
    ) (list_vertex g2) in
    VertexHashtbl.add mapping_vertex_g1_to_g2 vertex_copy original_vertex
  ) (list_vertex g1);

  mapping_vertex_g1_to_g2;;


let print_debug_graph g_original g_copie mapping msg path = 
  Zach.(
    print_string (msg ^ " :");
    nover_score g_copie;
    dot_output g_copie path;

    let components_copie = components g_copie in 
    let components = List.map ( 
      List.map (
        fun vertex -> 
          VertexHashtbl.find mapping vertex
      )
    ) components_copie in
    print_float (modularity g_original components);

    print_newline ()
  );;

(*
let test_g1  = 
  let g = Samplegraph.g1 in
  let g_copie = Zach.copy g in
  let mapping = map_vertex_between_graph g_copie g in

  let print_g = print_debug_graph g g_copie mapping in 
  print_g "g1 Iteration 0" "graphs/nover/g1-0.dot";
  step_nover g_copie;
  print_g "g1 Iteration 1" "graphs/nover/g1-1.dot";
  step_nover g_copie;
  print_g "g1 Iteration 2" "graphs/nover/g1-2.dot";
  ;;

let test_g2  = 
  let g = Samplegraph.g2 in
  let g_copie = Zach.copy g in
  let mapping = map_vertex_between_graph g_copie g in
  
  let print_g = print_debug_graph g g_copie mapping in 
  print_g "g2 Iteration 0" "graphs/nover/g2-0.dot";
  step_nover g_copie;
  print_g "g2 Iteration 1" "graphs/nover/g2-1.dot";
  step_nover g_copie;
  print_g "g2 Iteration 2" "graphs/nover/g2-2.dot";
  ;;


let test_g3  = 
  let g = Samplegraph.g3 in
  let g_copie = Zach.copy g in
  let mapping = map_vertex_between_graph g_copie g in
  
  let print_g = print_debug_graph g g_copie mapping in 
  print_g "g3 Iteration 0" "graphs/nover/g3-0.dot";
  step_nover g_copie;
  print_g "g3 Iteration 1" "graphs/nover/g3-1.dot";
  step_nover g_copie;
  print_g "g3 Iteration 2" "graphs/nover/g3-2.dot";
  step_nover g_copie;
  print_g "g3 Iteration 3" "graphs/nover/g3-3.dot";
  step_nover g_copie;
  print_g "g3 Iteration 4" "graphs/nover/g3-4.dot";
  ;;


let test_karat7  = 
  let g_copie = Zach.copy Zachgraph.karate in
  let mapping = map_vertex_between_graph g_copie Zachgraph.karate in

  let print_graph = print_debug_graph Zachgraph.karate g_copie mapping in 
  print_graph "Modularite Karate Itération 0" "graphs/nover/karate-nover-0.dot";

  step_nover g_copie;

  print_graph "Modularite Karate Itération 1" "graphs/nover/karate-nover-1.dot";

  step_nover g_copie;

  print_graph "Modularite Karate Itération 2" "graphs/nover/karate-nover-2.dot";

  step_nover g_copie;

  print_graph "Modularite Karate Itération 3" "graphs/nover/karate-nover-3.dot";
;;
*)