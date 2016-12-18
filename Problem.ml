open Core.Std

type t =
  | NoCode
  | BadCodeLen
  | NoStack
  | MmapFailed
  | MprotectFailed
  | VerificationFailed
[@@deriving show]
