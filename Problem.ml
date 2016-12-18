open Core.Std

type t =
  | NoCode
  | BadCodeLen
  | NoStack
  | MmapFailed
  | MprotectFailed
  | VerificationFailed of string
[@@deriving show]
