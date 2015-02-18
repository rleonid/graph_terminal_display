
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

