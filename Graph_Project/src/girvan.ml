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
	in let cleaned_results = List.fold_left
		(fun acc e ->
			(* si un élément est déjà présent, ne le rajoute pas *)
			if List.length
				(List.filter
					(fun e2 -> let(a, _) = e2 in let (b, _) = e in if e <> e2 && is_same_edge a b then true else false)
					acc)
				> 0
			then acc else e::acc
		) [] edges_with_numbers_list
	(* trie les arrêtes par indice décroissant *)
	in List.sort (fun (_, i) (_, i2) -> if i == i2 then 0 else if i < i2 then 1 else -1) cleaned_results

let find_edge_in_betweenness_list l e = List.fold_left (fun acc (e_internal, value) -> if is_same_edge e_internal e then (e, value)::acc else acc) [] l
let is_equal_betweenness_lists l1 l2 =
	List.length l1 = List.length l2
	&& List.fold_left
		(fun acc (edge, value) ->
			acc && (match find_edge_in_betweenness_list l2 edge with
				| [(_, value2)] -> value = value2
				| _ -> false))
		true
		l1

let%test _ = List.length (betweenness_score Samplegraph.g1) = 10
let%test _ = is_equal_betweenness_lists (betweenness_score Samplegraph.g1) [(Samplegraph.ed1_6, 12); (Samplegraph.ed1_9, 5); (Samplegraph.ed1_8, 5); (Samplegraph.ed1_2, 4); (Samplegraph.ed1_5, 4); (Samplegraph.ed1_4, 4); (Samplegraph.ed1_1, 1); (Samplegraph.ed1_3, 1); (Samplegraph.ed1_7, 1); (Samplegraph.ed1_10, 1)]
let%test _ = List.length (betweenness_score Samplegraph.g3) = 14
let%test _ = is_equal_betweenness_lists (betweenness_score Samplegraph.g3) [(Samplegraph.ed3_5, 40); (Samplegraph.ed3_6, 12); (Samplegraph.ed3_14, 7); (Samplegraph.ed3_7, 6); (Samplegraph.ed3_4, 5); (Samplegraph.ed3_2, 5); (Samplegraph.ed3_3, 5); (Samplegraph.ed3_1, 5); (Samplegraph.ed3_12, 3); (Samplegraph.ed3_9, 2); (Samplegraph.ed3_13, 2); (Samplegraph.ed3_8, 1); (Samplegraph.ed3_10, 1); (Samplegraph.ed3_11, 1)]

let rec girvan_newman g desired_n_com =
	if List.length (components g) < desired_n_com then
		match betweenness_score g with
			| [] -> ()
			| (e, _)::q ->
					begin
						remove_edge_e g e;
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
