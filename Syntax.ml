type instr =
  | Pop
  | Sub
  | Swap
  | Push of int
  | Jump of int
  | Jeq of int
  | Jlt of int
  | Stop

type prog = instr list
