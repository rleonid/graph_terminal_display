(* Use FaCiLe for chart positioning.*)
open Facile
open Easy

let just_x_layout msg_arr terminal_width =
  let num  = Array.length msg_arr in
  let x_lb = 0 and x_ub = terminal_width in
  let xpos = Fd.array ~name:"xpos" num x_lb x_ub in
  let slen = String.length in
  for i = 0 to num - 2 do
    Cstr.post (fd2e xpos.(i+1) >~ fd2e xpos.(i) +~ i2e (slen msg_arr.(i)));
  done;
  if Goals.solve (Goals.Array.labeling xpos) then
    for i = 0 to num - 1 do
      Printf.printf "%s: %d\n" msg_arr.(i) (Fd.elt_value xpos.(i))
    done
  else
    Printf.printf "failure\n"

let simple_x_and_y msg_arr terminal_width =
  let num  = Array.length msg_arr in
  let x_lb = 0 and x_ub = terminal_width in
  let y_lb = 0 and y_ub = num * 2 in              (* Flix XY over X axis *)
  let xpos = Fd.array ~name:"xpos" num x_lb x_ub in
  let ypos = Fd.array ~name:"ypos" num y_lb y_ub in
  let slen = String.length in
  for i = 0 to num - 2 do
    Cstr.post (fd2e xpos.(i+1) >~ fd2e xpos.(i) +~ i2e (slen msg_arr.(i)));
  done;
  Cstr.post (Alldiff.cstr ypos);
  let value = Fd.elt_value in
  let targ  = Array.append xpos ypos in
  if Goals.solve (Goals.Array.labeling targ) then
    let rec loop i acc =
      if i >= num then acc
      else loop (i + 1) ((value xpos.(i), value ypos.(i), msg_arr.(i))::acc)
    in
    loop 0 []
  (*for i = 0 to num - 1 do
      Printf.printf "%s: %d %d\n" msg_arr.(i) (value xpos.(i)) (value ypos.(i))
    done*)
  else
    [] (*Printf.printf "failure\n" *)


(* Each msg now has an array of integers, indexing back into the array,
   indicating precedense.
   Even though, other's may have precedense on the first element it will be
   displayed on top.
   *)
let with_predecessors msg_arr terminal_width =
  let num  = Array.length msg_arr in
  let x_lb = 0 and x_ub = terminal_width in
  let y_lb = 0 and y_ub = num * 2 in              (* Flix XY over X axis *)
  (* Variables *)
  let xpos = Fd.array ~name:"xpos" num x_lb x_ub in
  let ypos = Fd.array ~name:"ypos" num y_lb y_ub in
  (* Constraints *)
  Cstr.post (fd2e ypos.(0) =~ i2e 0);     (* First on top. *)
  for i = 0 to num - 1 do
    let others = snd msg_arr.(i) in
    (* 'On Top' means _lower_ y position. *)
    List.iter (fun oi ->
        let len = i2e (String.length (fst msg_arr.(oi)) + 1) in
        if oi <> 0 then Cstr.post (fd2e ypos.(i) <~ fd2e ypos.(oi));
        Cstr.post (fd2e xpos.(i) <~ fd2e xpos.(oi) -~ len))
        others
  done;
  Cstr.post (Alldiff.cstr ypos);
  let value = Fd.elt_value in
  let targ  = Array.append xpos ypos in
  if Goals.solve (Goals.Array.labeling targ) then
    let rec loop i acc =
      if i >= num then acc
      else loop (i + 1) ((value xpos.(i), value ypos.(i), (fst msg_arr.(i)))::acc)
    in
    loop 0 []
  else
    []

let display lst =
  List.sort (fun (_x1, y1, _l1) (_x2, y2, _l2) -> compare y1 y2) lst
  |> List.iter (fun (x, _, l) -> Printf.printf "%*s%s\n" x " " l)

let layout arr =
  display (with_predecessors arr 80)

let _ = layout [| "foo",[2;3]; "bar",[0;2]; "dog",[3] ; "cat",[]|] ;;
