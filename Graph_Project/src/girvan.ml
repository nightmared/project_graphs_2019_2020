open Zach.Zach

let is_same_edge e1 e2 =
	(E.dst e1 = E.dst e2 && E.src e1 = E.src e2)
	|| (E.src e2 = E.dst e1 && E.dst e2 = E.src e1)

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
		(* supprime les doublons (encore un algo en O(n²)) *)
	in List.fold_left
		(fun acc e ->
			(* si un élément est déjà présent, ne le rajoute pas *)
			if List.length
				(List.filter
					(fun e2 -> let(a, _) = e2 in let (b, _) = e in if e <> e2 && is_same_edge a b then true else false)
					acc)
				> 0
			then acc else e::acc)
		[]
		edges_with_numbers_list

let girvan_newman g n_com = 
	let b_score = betweenness_score g in ();;
