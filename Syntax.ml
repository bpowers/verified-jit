type instr =
  | Pop
  | Sub
  | Swap
  | Push of int
  | Jump of int
  | Jeq of int
  | Jlt of int
  | Stop
[@@deriving show]

type prog = instr list
[@@deriving show]
