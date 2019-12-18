open Zach.Zach

let betweenness_score g =
    let edges_in_shortest_paths = fold_vertex (fun v acc ->
        (fold_vertex (fun v_dist acc -> if v != v_dist then (fst (shortest_path g v v_dist))@acc else acc) g [])@acc
        ) g []
    in let sorted_edges_in_shortest_paths = List.sort (fun a b -> if a = b then 0 else -1) edges_in_shortest_paths
    in let edges_with_numbers_list = List.fold_left (fun acc e -> match acc with
        | (new_e, i)::q when new_e = e -> (e, i+1)::q
        | _ -> (e, 1)::acc
        ) [] sorted_edges_in_shortest_paths
    in edges_with_numbers_list;;

let girvan_newman g n_com = 
    let b_score = betweenness_score g in ();;
