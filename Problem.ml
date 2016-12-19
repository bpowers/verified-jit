open Core.Std

type t =
  | NoCode
  | BadCodeLen
  | NoStack
  | StackTooSmall
  | MmapFailed
  | MprotectFailed
  | VerificationFailed of string
[@@deriving show]
