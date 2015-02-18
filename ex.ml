
open Graph

(*
 * General graph structure:
 * - Vertices are strings.
 * - Edges are not labeled.
 * Graph is directed.
 *)
module Vertex = struct
  type t        = string
  let compare   = Pervasives.compare
  let equal     = (=)
  let hash      = Hashtbl.hash
  let create x  = x
  let label x   = x
end

module Gr = struct

  include Persistent.Digraph.Concrete(Vertex)

end

(*
 * Creating
let test_graph () =
 *)

let f  = "first" 
and s  = "second" 
and t  = "third"
and ft = "fourth"

let g1 = Gr.add_vertex Gr.empty f 
let g2 = Gr.add_vertex g1 s
let g3 = Gr.add_edge g2 f s
let g4 = Gr.add_edge g3 f t
let g5 = Gr.add_edge g4 f ft

(* What I wish i could do:
let test_graph2 () =
  let f  = "first" and s  = "second" in
  Gr.add_vertex Gr.empty f
  |> Gr.add_vertex s
  |> Gr.add_edge f s
  *)


(*
 * Outputting to a terminal
 *)

(* Common UTF8 codes for single characters. *)
let left_arrow  = "\xE2\x86\x90"
let right_arrow = "\xE2\x86\x92"
let right_tack  = "\xE2\x8A\xA2"
let left_tack   = "\xE2\x8A\xA3"

let left_ceiling  = "\xE2\x8C\x88"
let right_celing  = "\xE2\x8C\x89"
let left_floor    = "\xE2\x8C\x8A"
let right_floor   = "\xE2\x8C\x8B"

let point_to_left1  = left_floor ^ right_arrow
let point_to_right1 = left_arrow ^ right_floor

let point_to_left2  = left_floor ^ "-"
let point_to_right2 = "-" ^ right_floor

let output_vertex g v =
  Printf.printf "%s\n" v;
  Gr.iter_succ (Printf.printf "%s%s\n" point_to_left2) g v;
  Gr.iter_pred (Printf.printf "%s%s\n" point_to_right2) g v;
  ()

let get_vertex g =
  Gr.fold_vertex (fun x -> function | None -> Some x | Some x as r -> r) g None
  |> function
     | None   -> raise (Invalid_argument "Invalid_graph")
     | Some v -> v

let output g = output_vertex g (get_vertex g)


