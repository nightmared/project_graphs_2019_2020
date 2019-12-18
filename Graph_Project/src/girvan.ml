open Zach.Zach

let betweenness_score g =
    let edges_in_shortest_paths = fold_vertex (fun v acc ->
        (fold_vertex (fun v_dist acc -> if v != v_dist then (fst (shortest_path g v v_dist))@acc else acc) g [])@acc
        ) g []
    in let sorted_edges_in_shortest_paths = List.sort (fun a b -> if a = b then 0 else -1) edges_in_shortest_paths
    in ();;

let girvan_newman g n_com = ();;
