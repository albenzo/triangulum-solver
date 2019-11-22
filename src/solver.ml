open Graph;;

module G = Persistent.Graph.Abstract(struct type t = int end);;
module GTraverse = Traverse.Dfs(G);;
module GNbhd = Oper.Neighbourhood(G);;

type weight_graph = G.t * (G.V.t -> int);;

let weight (g, w) = GTraverse.fold (fun v sum -> (w v) + sum) 0 g;;
let propagate_vertex (g, w) v =
  (G.remove_vertex g v),
  (fun u -> if GNbhd.Vertex_Set.mem v (GNbhd.set_from_vertex g u)
            then (w u) + (w v)
            else (w u));;

let left (x,y) = x;;
let right (x,y) = y;;
let max_f f x y = if (f x) > (f y) then x else y;;

let maximal_propagated_removal (g, w) =
  let rec max_prop_stack (g,w) removed =
    GTraverse.fold
      (fun v gval ->
        max_f
          left
          gval
          (max_prop_stack (propagate_vertex (g,w) v) (v::removed))) (0,[]) g
  in max_prop_stack (g,w) [];;

let graph_from_lists verts edges =
  List.fold_left
    (fun g e -> G.add_edge g
                  (G.V.create (left e))
                  (G.V.create (right e)))
    (List.fold_left (fun g v -> G.add_vertex g (G.V.create v)) G.empty verts)
    edges;;


let g3 = graph_from_lists [1;2;3;4;5;6;7;8;9;10] [(1,3);(2,3);(2,6);(3,4);(5,6);(6,7);(7,10);(8,9)];;
let w3 x = if G.V.label x = 7 then 2 else 1;;
let gw3 = g3,w3;;
let g1 = graph_from_lists [1;2;3;4] [(1,3);(2,3);(3,4)];;
let one x = 1;;
let g_min = graph_from_lists [1;2;3] [(1,2);(2,3)];;
