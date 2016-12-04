open Core.Std

type instr =
  | Pop
  | Sub
  | Swap
  | Push of int
  | Jump of int
  | Jeq of int
  | Jlt of int
  | Stop

let eval (bytecode : string) (args : int list) =
  print_string bytecode

let spec =
  let open Command.Spec in
  empty
  +> anon ("bytecode" %: string)
  +> anon (sequence ("number" %: int))

let main =
  Command.basic
    ~summary:"Run a bytecode program"
    ~readme:(fun () -> "Arguments are the bytecode program followed by ints to push onto the stack")
    spec
    (fun bytecode args () -> eval bytecode args)

let () =
  Command.run ~version:"0.1.0" main
