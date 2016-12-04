open Syntax

let rec fetch (n: int) (cs: prog): instr option =
  match n, cs with
  | _, [] -> None
  | n, (c :: cs) -> if n = 0
                    then Some c
                    else fetch (n-1) cs

(**
  next encodes the operational semantics from Section 3
  xs: data stack (list of 32-bit words
  l: natural number representing available stack space
  p: bytecode program counter
  cs: byecode program
*)
let next (xs: int list) (l: int) (p: int) (cs: prog) =
  match fetch p cs with
  | Some Pop      -> (match xs with
                      | x :: y :: xs -> (y :: xs, l+1, p+1, cs)
                      | _            -> failwith "stuck in pop")
  | Some Sub      -> (match xs with
                      | x :: y :: xs -> ((x - y) :: y :: xs, l, p+1, cs)
                      | _            -> failwith "stuck in sub")
  | Some Swap     -> (match xs with
                      | x :: y :: xs -> (y :: x :: xs, l, p+1, cs)
                      | _            -> failwith "stuck in swap")
  | Some (Push i) -> if l = 0
                     then failwith "stuck (max stack depth exceeded)"
                     else (i :: xs, l-1, p+1, cs)
  | Some (Jump i) -> (xs, l, i, cs)
  | Some (Jeq i)  -> (match xs with
                      | x :: y :: _ -> if x = y
                                       then (xs, l, i, cs)
                                       else (xs, l, p+1, cs)
                      | _           -> failwith "stuck in jeq")
  | Some (Jlt i)  -> (match xs with
                      | x :: y :: _ -> if x < y
                                       then (xs, l, i, cs)
                                       else (xs, l, p+1, cs)
                      | _           -> failwith "stuck in jlt")
  | Some Stop     -> failwith "stuck (unexpected stop)"
  | None          -> failwith (Printf.sprintf "stuck (bad fetch of %d)" p)

(**
  exec describes the effect of successfully executing a bytecode program
*)
let rec exec (xs: int list) (l: int) (p: int) (cs: prog) =
  match fetch p cs with
  | Some Stop -> (xs, l, p, cs)
  | _ -> let (xs, l, p, cs) = next xs l p cs in
         exec xs l p cs
