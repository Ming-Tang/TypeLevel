namespace TypeLevel

[<AutoOpen>]
module List =
  type List = interface end

  type E = E with interface List
  type C<'a, 'b> = C of 'a * 'b with interface List

  type Head = Head with
    interface Func
    static member inline (<|-) (Head, C(a, b)) = a

  type Tail = Tail with
    interface Func
    static member inline (<|-) (Tail, C(a, b)) = b

  type E with
    static member inline (<||-) (f, E) = E
    static member inline (<|?-) (f, E) = E
    static member inline (<|/-) ((f, x0), E) = x0
    static member inline (!++) (E) = N0

  type C<'a, 'b> with
    static member inline (<||-) (f, C(a, b)) =
      C(f <|- a, f <||- b)

    static member inline (<|?-) (f, C(a, b)) =
      (f <|- a) <?> (C(a, f <|?- b), f <|?- b)

    static member inline (<|/-) ((f, x0), C(a, b)) =
      f <|- (a, (f, x0) <|/- b)

    static member inline (!++) (C(_, b)) = S(!++ b)

  type Length = Length with
    static member inline (<|-) (Length, xs) = !++ xs

  type Map<'f> = Map of 'f with
    interface Func
    static member inline (<|-) (Map f, xs) = f <||- xs

  type Filter<'f> = Filter of 'f with
    interface Func
    static member inline (<|-) (Filter f, xs) = f <|?- xs

  type Fold<'f, 'x0> = Fold of 'f * 'x0 with
    interface Func
    static member inline (<|-) (Fold(f, x0), xs) = (f, x0) <|/- xs
