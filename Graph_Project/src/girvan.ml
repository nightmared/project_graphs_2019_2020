open Zach.Zach

let is_same_edge e1 e2 =
	(E.dst e1 = E.dst e2 && E.src e1 = E.src e2)
	|| (E.src e2 = E.dst e1 && E.dst e2 = E.src e1)

let is_in_list e l = 
	List.fold_left (fun acc ei -> acc || (is_same_edge ei e)) false l

let betweenness_score_internal g vertex_list =
	let edges_with_doubles = List.fold_left (fun acc v -> (succ_e g v)@acc) [] vertex_list
	(* suppression des doublons *)
	in let edges = List.fold_left (fun acc e -> if not (is_in_list e acc) then e::acc else acc) [] edges_with_doubles
	in let _ = List.iter (fun e -> setscore 0. e) edges
	(* pour tous les noeuds, énumère le chemin le plus court vers les autres noeuds *)
	in let edges_in_shortest_paths =
		List.fold_left
			(fun acc v ->
				(List.fold_left (fun acc v_dist -> if v != v_dist then (fst (shortest_path g v v_dist))@acc else acc) [] vertex_list)@acc
			) [] vertex_list
	in begin
		List.iter (fun e -> setscore (getscore e +. 1.) e) edges_in_shortest_paths;
		List.iter (fun e -> setscore (getscore e /. 2.) e) edges;
		()
	end

(* Run the betweenness_score algorithm across all components separately *)
let betweenness_score g =
	List.iter (fun e -> betweenness_score_internal g e) (components g)

let%test _ = begin betweenness_score Samplegraph.g1; getscore Samplegraph.ed1_6 = 12. && getscore Samplegraph.ed1_9 = 5. && getscore Samplegraph.ed1_8 = 5. && getscore Samplegraph.ed1_2 = 4. && getscore Samplegraph.ed1_5 = 4. && getscore Samplegraph.ed1_4 = 4. && getscore Samplegraph.ed1_1 = 1. && getscore Samplegraph.ed1_3 = 1. && getscore Samplegraph.ed1_7 = 1. && getscore Samplegraph.ed1_10 = 1. end
let%test _ = begin betweenness_score Samplegraph.g3; getscore Samplegraph.ed3_5 = 20. && getscore Samplegraph.ed3_6 = 12. && getscore Samplegraph.ed3_14 = 7. && getscore Samplegraph.ed3_7 = 6. && getscore Samplegraph.ed3_4 = 5. && getscore Samplegraph.ed3_2 = 5. && getscore Samplegraph.ed3_3 = 5. && getscore Samplegraph.ed3_1 = 5. && getscore Samplegraph.ed3_12 = 3. && getscore Samplegraph.ed3_9 = 2. && getscore Samplegraph.ed3_13 = 2. && getscore Samplegraph.ed3_8 = 1. && getscore Samplegraph.ed3_10 = 1. && getscore Samplegraph.ed3_11 = 1. end

let rec girvan_newman g desired_n_com =
	begin
		betweenness_score g;
		if List.length (components g) < desired_n_com then
			let e = fold_edges_e (fun e acc -> if getscore e > getscore acc then e else acc) g (choosee g)
			in remove_edge_e g e;
			girvan_newman g desired_n_com
	end

let copy_g1 = copy Samplegraph.g1
let list_edges_g1 = fold_edges_e (fun e acc -> e::acc) copy_g1 []
let copy_g3 = copy Samplegraph.g3
let list_edges_g3 = fold_edges_e (fun e acc -> e::acc) copy_g3 []
let list_edges_g3_girvan2 = List.filter (fun e -> Edge.number (E.label e) <> 5) list_edges_g3
let find_edge_in_list l e = List.fold_left (fun acc e_internal -> if is_same_edge e_internal e then true else acc) false l
let is_equal_lists l1 l2 =
	List.length l1 = List.length l2
	&& List.fold_left
		(fun acc edge -> acc && find_edge_in_list l2 edge)
		true
		l1

let%test _ = begin girvan_newman copy_g1 1; is_equal_lists list_edges_g1 (fold_edges_e (fun e acc -> e::acc) copy_g1 []) end = true
let%test _ = begin girvan_newman copy_g3 2; is_equal_lists list_edges_g3_girvan2 (fold_edges_e (fun e acc -> e::acc) copy_g3 []) end = true
