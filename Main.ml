open Core.Std
open Syntax

let max_stack_depth = 32

let parse (bytecode: string) : prog =
  Parser.program Lexer.token (Lexing.from_string bytecode)

(* encodes a given program as an ASCII string *)
let rec encode (prog: prog) : string =
  let imm (i : int) =
    Printf.sprintf "%d" i
  in
  let enc (instr: instr) =
    match instr with
    | Pop    -> "p"
    | Sub    -> "-"
    | Swap   -> "s"
    | Push i -> "c" ^ imm i
    | Jump i -> "j" ^ imm i
    | Jeq i  -> "=" ^ imm i
    | Jlt i  -> "<" ^ imm i
    | Stop   -> "."
  in
  match prog with
  | [] -> ""
  | instr :: prog -> (enc instr) ^ (encode prog)

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
  | Some (Push i) -> (i :: xs, l-1, p+1, cs)
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


let eval (bytecode: string) (args: int list) : int option =
  let xs = args in
  let l = max_stack_depth - (List.length args) in
  let p = 0 in
  let cs = parse bytecode in
  let (stack, _, _, _) = exec xs l p cs in
  let open Printf in
  printf "stack: [";
  List.iter stack (printf "%d ");
  printf "]\n";
  printf "enc: %s (orig: %s)\n" (encode (parse bytecode)) bytecode;
  match stack with
  | result :: _ -> Some result
  | _ -> None

let spec =
  let open Command.Spec in
  empty
  +> anon ("bytecode" %: string)
  +> anon (sequence ("number" %: int))

let main =
  Command.basic
    ~summary:"Run a bytecode program"
    ~readme:(fun () -> "JIT compiles and executes x86 instructions corresponding to the bytecode.")
    spec
    (fun bytecode args () -> (match eval bytecode args with
                              | Some result -> printf "Result: %d\n" result
                              | None        -> printf "Executed fully, but nothing on stack."))

let () =
  Command.run ~version:"0.1.0" main
