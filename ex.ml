
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
and fi = "fifth"

(* Some graphs: *)
let g1 = Gr.add_vertex Gr.empty f
let g2 = Gr.add_vertex g1 s
let g3 = Gr.add_edge g2 f s
let g4 = Gr.add_edge g3 f t
let g5 = Gr.add_edge g4 ft f
let g6 = Gr.add_edge g4 ft s
let g7 = Gr.add_edge g5 s fi

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
let vertical_bar  = '|'

let point_to_left1  = left_floor ^ right_arrow
let point_to_right1 = left_arrow ^ right_floor

let point_to_left2  = left_floor ^ "-"
let point_to_right2 = "-" ^ right_floor

type line = { left_arrow      : bool      (* point from left? *)
            ; left_offset     : int       (* Amount of space on the left. *)
            ; text            : string    (* What to write. *)
            ; right_offset    : int       (* Add arrow automatically. *)
            ; right_verticals : int list  (* Where do we add right vertical lines *)
            }

let make_line ?(left_arrow=false) ?(left_offset=0) ?(right_offset=0)
    ?(right_verticals=[]) text =
        { left_arrow      = left_arrow
        ; left_offset     = left_offset
        ; text            = text
        ; right_offset    = right_offset
        ; right_verticals = right_verticals
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
      let r = right_arrow ^ String.make (l.right_offset) '-' ^ right_celing in
      List.iter (fun i -> r.[i] <- vertical_bar) l.right_verticals;
      r
    else
      List.fold_left (fun s i -> s ^ String.make i ' ' ^ String.make 1 vertical_bar)
        "" l.right_verticals
  in
  Printf.fprintf oc "%s%s%s\n" start l.text stop

let buffer_width = ref 1

let make_lines g v =
    let create_lines fold make =
        fold (fun v (len, lst) ->
            (max len (String.length v), make v :: lst))
            g v (0, [])
    in
    let first_line    = make_line v in
    let first_length  = String.length first_line.text + !buffer_width in
    let make_succ     = make_line ~left_arrow:true
                          ~left_offset:(first_length - 2)
    in
    let (ml, sllst)   = create_lines Gr.fold_succ make_succ in
    let ml            = ml + !buffer_width in
    let make_pred     = make_line ~left_arrow:false
                          ~left_offset:(first_length + ml)
    in
    let ml2, pllst    = create_lines Gr.fold_pred make_pred in
    (*Printf.printf "ml: %d ml2: %d\n" ml ml2; *)
    if List.length pllst = 0
    then first_line :: sllst
    else { first_line with right_offset = ml } ::
          (List.map (fun l -> {l with right_verticals = ml - (String.length l.text) :: []}) sllst)
          @ pllst

(* TODO: I am making assumptions that the graphs are Acyclical! *)

module M = Map.Make (struct type t = string let compare = compare end)

let assign_columns g v =
  let n = Gr.nb_vertex g in
  let q = Queue.create () in
  let add_to_queue m c v = if not (M.mem v m) then Queue.add (v, c) q in
  Queue.add (v, 0) q;
  let rec loop m =
    if M.cardinal m = n     (* assigned everyone *)
    then M.bindings m
    else
      let (v, c) = Queue.pop q in
      List.iter (add_to_queue m (c + 1)) (Gr.succ g v);
      List.iter (add_to_queue m (c - 1)) (Gr.pred g v);
      loop (M.add v c m)
  in
  loop (M.singleton v 0)

(* Create a precedence array.  *)
let to_precedence_lst g v =
  let n = Gr.nb_vertex g in
  let q = Queue.create () in
  let add_to_queue m v = if not (M.mem v m) then Queue.add v q in
  Queue.add v q;
  let rec report m =
    M.bindings m
    |> List.sort (fun (_, (o1, _)) (_, (o2, _)) -> compare o1 o2)
    |> List.map (fun (v, (_, plst)) -> (v, plst))
  and loop c m =
    if M.cardinal m = n then (* assigned everyone *)
      report m
    else if Queue.is_empty q then
      begin
        Printf.eprintf "Warning: graph is not connected!\n";
        report m
      end
    else
      let vertex = Queue.pop q in
      let v_succ = Gr.succ g vertex in
      let v_pred = Gr.pred g vertex in
      let m'     = M.add vertex (c, v_succ) m in
      List.iter (add_to_queue m') v_pred;
      List.iter (add_to_queue m') v_succ;
      loop (c + 1) m'
  in
  loop 0 M.empty

let constrained_lines g v =
  let as_prcdlst = to_precedence_lst g v in
  let aligned    = Position.align_by_precedence as_prcdlst ~terminal_width:80
                      ~display_padding:2 in
  let sorted     = List.sort (fun (_, (_, y1)) (_, (_, y2)) -> compare y1 y2) aligned in
  let as_lines   = List.map (fun (l, (x, _)) -> make_line ~left_offset:x l) sorted in
  List.iter (output_line stdout) as_lines

let output_vertex_old g v = List.iter (output_line stdout) (make_lines g v)

let get_vertex g =
  Gr.fold_vertex (fun x -> function | None -> Some x | Some x as r -> r) g None
  |> function
     | None   -> raise (Invalid_argument "Invalid_graph")
     | Some v -> v

(*let output g = output_vertex g (get_vertex g) *)
let output g = constrained_lines g (get_vertex g)

let test n g =
  Printf.printf "ouputing: %d\n" n;
  output g

let () = test 1 g1
let () = test 2 g2
let () = test 3 g3
let () = test 4 g4
let () = test 5 g5
let () = test 6 g6
let () = test 7 g7

