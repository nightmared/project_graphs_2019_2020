open Zach.Zach

let is_same_edge e1 e2 =
	(E.dst e1 = E.dst e2 && E.src e1 = E.src e2)
	|| (E.src e2 = E.dst e1 && E.dst e2 = E.src e1)

let compare_edge e1 e2 =
	if is_same_edge e1 e2 then 0 else compare e1 e2

let betweenness_score g =
	(* pour tous les noeuds, énumère le chemin le plus court vers les autres noeuds *)
	let edges_in_shortest_paths = fold_vertex (fun v acc ->
		(fold_vertex (fun v_dist acc -> if v != v_dist then (fst (shortest_path g v v_dist))@acc else acc) g [])@acc
		) g []
	(* trie cette liste par arrête *)
	in let sorted_edges_in_shortest_paths = List.sort compare edges_in_shortest_paths
	(* compte le nombre de fois où chaque arrête est présente *)
	in let edges_with_numbers_list = List.fold_left (fun acc e -> match acc with
		| (new_e, i)::q when is_same_edge e new_e -> (e, i+1)::q
		| _ -> (e, 1)::acc
		) [] sorted_edges_in_shortest_paths
	in edges_with_numbers_list;;

let girvan_newman g n_com = 
	let b_score = betweenness_score g in ();;
