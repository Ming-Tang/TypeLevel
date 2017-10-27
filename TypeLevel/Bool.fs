namespace TypeLevel

[<AutoOpen>]
module Bool =
  type Bool = interface end

  type False = False with
    interface Bool
    static member inline (<?>) (False, (x, y)) = y

  type True = True with
    interface Bool
    static member inline (<?>) (True, (x, y)) = x

  type If<'c, 'p, 'q> = If of 'c * 'p * 'q with
    interface Func
    static member inline (<|-) (If(c, p, q), v) =
      ((c <|- v) <?> (p, q)) <|- v

  type If0<'c, 'p> = If0 of 'c * 'p with
    interface Func
    static member inline (<|-) (If0(c, p), v) =
      ((c <|- v) <?> (p, Id)) <|- v

  type Not = Not with
    interface Func
    static member inline (<|-) (Not, True) = False
    static member inline (<|-) (Not, False) = True

  type While<'p, 'f> = While of 'p * 'f with
    interface Func
    static member inline (<|-) (While(p, f), v) =
      ((p <|- v) <?> (While(p, f) <-< f, Id)) <|- v
      //If0(p, f >-> If0(p, f >-> If0(p, f >-> If0(p, If0(p, f >-> If0(p, f >-> While(p, f))))))) <|- v

  type Or = Or with
    interface Func
    static member inline (<|-) (Or, (True, True)) = True
    static member inline (<|-) (Or, (True, False)) = True
    static member inline (<|-) (Or, (False, True)) = True
    static member inline (<|-) (Or, (False, False)) = False

  type And = And with
    interface Func
    static member inline (<|-) (And, (True, True)) = True
    static member inline (<|-) (And, (True, False)) = False
    static member inline (<|-) (And, (False, True)) = False
    static member inline (<|-) (And, (False, False)) = False
