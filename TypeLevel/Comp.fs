namespace TypeLevel

[<AutoOpen>]
module Comp =
  type CompState<'Code, 'Data> = CompState of 'Code * 'Data

  type Instr = interface end

  type End = End with interface Instr
  type Nop = Nop with interface Instr
  type App<'f> = App of 'f with interface Instr
  type Jmp<'p> = Jmp of 'p with interface Instr
  type CondJmp<'p> = CondJmp of 'p with interface Instr
  type Move<'n> = Move of 'n with interface Instr

  type Nop with
    static member inline (<!!!>) (Nop, CompState(code, data)) =
      CompState(code, data), True

    static member inline (!?) Nop = True

  type App<'f> with
    static member inline (<!!!>) (App f, CompState(code, Tape(l, x, r))) =
      CompState(code, Tape(l, f <|- x, r)), True

    static member inline (!?) (App _) = True

  type Jmp<'p> with
    static member inline (<!!!>) (Jmp p, CompState(code, data)) =
      CompState(p <|- code, data), False

    static member inline (!?) (Jmp _) = True

  type CondJmp<'p> with
    static member inline (<!!!>) (CondJmp p,
                                  (CompState(code, (Tape(_, x, _) as data)) as state)) =
      (!? x) <?> ((CompState(p <|- code, data), False),
                  (state, True))

    static member inline (!?) (CondJmp _) = True

  type Move<'n> with
    static member inline (<!!!>) (Move n, CompState(code, data)) =
      CompState(code, n <|- data), True

    static member inline (!?) (Move _) = True

  type End with
    static member inline (<!!!>) (End, CompState(code, data)) =
      CompState(code, data), False

    static member inline (!?) (End _) = False

  type CompState<'Code, 'Data> with
    static member inline (!!!!) (CompState(code, data) as state) =
      let i = !. code
      let (CompState(code', data')), t = i <!!!> state
      CompState(t <?> ((ShiftRight <|- code'), code'), data')

    static member inline (!..) (CompState(code, data)) = !. code


  type Adv = Adv with
    // optimization: check condition once per 5 times
    static member inline (<|-) (Adv, state) = !!!!! !!!! !!!! !!!! !!!! !!!! state

  type CompState<'Code, 'Data> with
    static member inline (<!!!!>) (Z, state) = state
    static member inline (<!!!!>) (S x, state) = x <!!!!> (!!!! state)

    static member inline (!!!!!) state =
      ((!? !.. state) <?> (Adv, Id)) <|- state

module TestApp =
  let state1 = CompState(Tape(E, App(Succ), L2(App(Succ), End)),
                         Tape(E, N3, E))
  let state2 = !!!! state1
  let state3 = !!!! state2
  let state4 = !!!! state3

module TestMove =
  let state1 = CompState(Tape(E, Move(ShiftRight), L2(Move(ShiftLeft), End)),
                         Tape(E, N3, L2(N2, N1)))
  let state2 = !!!! state1
  let state3 = !!!! state2

module TestJmp =
  let state1 = CompState(Tape(E, Jmp(Rep(N4, ShiftRight)),
                              L5(Nop, Nop, Nop, App(Succ), End)),
                         Tape(E, N3, E))
  let state2 = !!!! state1

module TestCondJmp =
  let t1 = (!? N3) <?> (N10, N1)
  let t2 = (!? N0) <?> (N10, N1)
  let state1 = CompState(Tape(E, CondJmp(Rep(N4, ShiftRight)),
                              L5(App(Pred), Nop, Nop, App(Succ), End)),
                         Tape(E, N3, E))
  let state2 = !!!! state1

  let state1a = CompState(Tape(E, CondJmp(Rep(N4, ShiftRight)),
                               L5(App(Pred), Nop, Nop, App(Succ), End)),
                          Tape(E, N0, E))
  let state2a = !!!! state1a

  let final = !!!!! state1

module TestLoop =
  // x = 0, y = 4;
  // ^start
  // 0 <move right>
  // 1 label: y--;
  // 2        <move left>
  // 3        x++;
  // 4        <move right>
  // 5        if (y) goto label;
  // 6 <end>
  // x = 4, y = 0;
  //        ^end
  let state1 = CompState(Tape(E,
                              Move(ShiftRight),
                              L6(App(Pred),
                                 Move(ShiftLeft),
                                 App(Succ),
                                 Move(ShiftRight),
                                 CondJmp(Rep(N4, ShiftLeft)),
                                 End)),
                         Tape(E, N0, L1(N4)))
  let state2 = !!!! state1
  let state3 = !!!! state2
  let state4 = !!!! state3
  let state5 = !!!! state4
  let state6 = !!!! state5
  let state7 = !!!! state6
  let state8 = !!!! state7
  let state9 = !!!! state8
  let state10 = !!!! state9
  let state11 = !!!! state10
  let state12 = !!!! state11
  let state13 = !!!! state12
  let state14 = !!!! state13
  let state15 = !!!! state14
  let state16 = !!!! state15
  let state17 = !!!! state16
  let state18 = !!!! state17
  let state19 = !!!! state18
  let state20 = !!!! state19
  let state21 = !!!! state20
  let state22 = !!!! state21

  let i21 : CondJmp<_> = !.. state21
  let b21 : True = !? i21
  let i22 : End = !.. state22
  let b22 : False = !? i22

  //let n22 = (N10 + (N10 + N2))
  //let s10 = N10 <!!!!> state1
  //let s20 = N10 <!!!!> s10
  //let s22 = N2 <!!!!> s20
  let final = !!!!! state12

