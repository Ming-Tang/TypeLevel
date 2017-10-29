namespace TypeLevel

[<AutoOpen>]
module Tape =
  type Tape<'l, 'x, 'r> = Tape of 'l * 'x * 'r with
    static member inline (!>>>) (Tape(l, x, E)) = Tape(l, x, E)
    static member inline (!>>>) (Tape(l, x, C(a, r))) = Tape(C(x, l), a, r)
    static member inline (!<<<) (Tape(C(a, l), x, r)) = Tape(l, a, C(x, r))
    static member inline (!<<<) (Tape(E, x, r)) = Tape(E, x, r)
    static member inline (!.) (Tape(_, x, _)) = x

  type ShiftLeft = ShiftLeft with
    interface Bool
    static member inline (<|-) (ShiftLeft, x) = !<<< x

  type ShiftRight = ShiftRight with
    interface Bool
    static member inline (<|-) (ShiftRight, x) = !>>> x

