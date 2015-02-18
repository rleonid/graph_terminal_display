
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
 *)
let test_graph () =
  let f  = "first"
  and s  = "second" in
  let g1 = Gr.add_vertex Gr.empty f  in
  let g2 = Gr.add_vertex g1 s in
  let g3 = Gr.add_edge g2 f s in
  g3

(* What I wish i could do:
let test_graph2 () =
  let f  = "first" and s  = "second" in
  Gr.add_vertex Gr.empty f
  |> Gr.add_vertex s
  |> Gr.add_edge f s
  *)

(*
 * Outputting to a .dot file.
 *)
module DotVis = struct
  include Gr
  (* Not adding any special attributes to our graph visualization,
     only exporting the vertex names. *)
  let graph_attributes          _g  = []
  let default_vertex_attributes _g  = []
  let vertex_name               vt  = vt
  let vertex_attributes         _vt = []
  let get_subgraph              _vt = None
  let default_edge_attributes   _g  = []
  let edge_attributes           _et = []

end

module Gv = Graphviz.Dot(DotVis)

let to_dot g fname =
  let oc = open_out fname in
  (*let fo = Format.formatter_of_out_channel oc in
  let () = Gv.fprint_graph fo g in *)
  let () = Gv.output_graph oc g in
  flush oc;
  close_out oc


(*
 * Outputting to a terminal
 *)

