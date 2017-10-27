namespace TypeLevel

[<AutoOpen>]
module Functional =
  type Func = interface end

  type Id = Id with
    interface Func
    static member inline (<|-) (Id, x) = x

  type Fst = Fst with
    interface Func
    static member inline (<|-) (Fst, (x, y)) = x

  type Snd = Snd with
    interface Func
    static member inline (<|-) (Snd, (x, y)) = y

  type Const<'k> = Const of 'k with
    interface Func
    static member inline (<|-) (Const(k), x) =
      k

  /// Function composition
  type Comp<'a, 'b> = Comp of 'a * 'b with
    static member inline (<|-) (Comp(f, g), x) =
      f <|- (g <|- x)

  type Comp3<'a, 'b, 'c> = Comp<'a, Comp<'b, 'c>>

  let inline c3 a b c : Comp3< ^a, ^b, ^c> = Comp(a, Comp(b, c))

  /// 2-way fork into tuple
  type Fork<'a, 'b> = Fork2 of 'a * 'b with
    interface Func
    static member inline (<|-) (Fork2(a, b), x) : ^aa * ^bb =
      (a <|- x), (b <|- x)

  /// 3-way fork into tuple
  type Fork<'a, 'b, 'c> = Fork3 of 'a * 'b * 'c with
    interface Func
    static member inline (<|-) (Fork3(a, b, c), x) =
      (a <|- x), (b <|- x), (c <|- x)

  type Partial<'f, 'x> = Partial of 'f * 'x with
    interface Func
    static member inline (<|-) (Partial(f, x), y) =
      f <|- (x, y)

  type PartialRight<'f, 'y> = PartialRight of 'f * 'y with
    interface Func
    static member inline (<|-) (PartialRight(f, y), x) =
      f <|- (x, y)

  let inline (-|>) x f = f <|- x
  let inline (<-<) f g = Comp(f, g)
  let inline (>->) g f = Comp(f, g)

  let inline f2 (a, b) = Fork2(a, b)
  let inline (<*>) a b = Fork2(a, b)
  let inline f3 (a, b, c) = Fork3(a, b, c)
  let inline (<&>) a b = Fork2(Fst >-> a, Snd >-> b)
