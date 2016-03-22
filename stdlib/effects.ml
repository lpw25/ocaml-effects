(*
external perform : 'a eff -> 'a = "%perform"

type ('b,'c) handler =
   { eff: 'a. 'a eff -> ('a, 'c) continuation -> 'c;
     exn: exn -> 'c;
     return: 'b -> 'c; }

external handle_raw : ('a -> 'b) -> ('b -> 'c) -> (exn -> 'c) ->
                      ('d eff -> ('d, 'c) continuation -> 'c) -> 'c = "%handle"


let handle {eff; exn; return} f x =
  handle_raw (fun () -> f x) return exn eff

external continue: ('a, 'b) continuation -> 'a -> 'b = "%continue"

external discontinue: ('a, 'b) continuation -> exn -> 'b = "%discontinue"

let delegate e k =
  match perform e with
  | v -> continue k v
  | exception e -> discontinue k e

let default (eff : 'a eff) : 'a =
  let slot = Obj.extension_slot eff in
  if Obj.size slot < 3 then raise Unhandled
  else begin
   let def = (Obj.obj (Obj.field slot 2) : 'a eff -> 'a) in
     def eff
  end

(*
type ('a, 'b) stack
external bvar_of_cont : ('a, 'b) continuation -> ('a, 'b) stack Domain.BVar.t = "%identity"
external resume : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%resume"

let continue' k v = resume (Domain.BVar.take (bvar_of_cont k)) (fun x -> x) v
let discontinue' k e = resume (Domain.BVar.take (bvar_of_cont k)) (fun e -> raise e) e
*)
*)
