
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

(* Some graphs: *)
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

type line = { left_arrow   : bool      (* point from left? *)
            ; left_offset  : int       (* Amount of space on the left. *)
            ; text         : string    (* What to write. *)
            ; right_offset : int       (* Add arrow automatically. *)
            }

let make_line ?(left_arrow=false) ?(left_offset=0) ?(right_offset=0) text =
  { left_arrow    = left_arrow
  ; left_offset   = left_offset
  ; text          = text
  ; right_offset  = right_offset
  }

let output_line oc l =
  let start = 
    if l.left_arrow then
      point_to_left2 ^ (String.make l.left_offset '-')
    else
      String.make l.left_offset ' '
  in
  let stop =
    if l.right_offset > 0 then
      (right_arrow ^ String.make (l.right_offset - 2) '-' ^ right_celing)
    else
      ""
  in
  Printf.fprintf oc "%s%s%s\n" start l.text stop

let make_lines g v =
  let first_line    = make_line v in
  let first_offset  = String.length first_line.text - 2 in
  let make_succ     = make_line ~left_arrow:true ~left_offset:first_offset in
  let succ_lines    = Gr.fold_succ (fun v l -> make_succ v :: l) g v [] in
  first_line :: succ_lines

let output_vertex g v =
  List.iter (output_line stdout) (make_lines g v)

let get_vertex g =
  Gr.fold_vertex (fun x -> function | None -> Some x | Some x as r -> r) g None
  |> function
     | None   -> raise (Invalid_argument "Invalid_graph")
     | Some v -> v

let output g = output_vertex g (get_vertex g)


