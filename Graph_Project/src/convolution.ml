 
let sum_list_of_floats = List.map2 (+.);;

let convolution g ver = 
  Zach.(
    let parameter_sum = Zach.fold_succ(fun v sum ->
        sum_list_of_floats sum (Zach.getparamv v)
    ) g ver (Zach.getparamv ver)
    in
    List.map (fun x -> x /. float_of_int(Zach.in_degree g ver + 1)) parameter_sum
  );;

let %test "convolution g4 v4_4" = 
  Samplegraph.(
    convolution g4 v4_4 = [1.5;2.0;1.0]
  );;
  

let print_param g = 
  Zach.(
    Zach.iter_vertex(fun v -> 
      Printf.printf "Node %d : " (Zach.Vertex.number (Zach.V.label v));
      (let param = convolution g v in
      List.iter (fun x -> Printf.printf "%.4g\t" x) param);
      print_newline ();
    ) g
  );;

(*
let test = 
  Samplegraph.(
    print_param Samplegraph.g4
  );;
*)