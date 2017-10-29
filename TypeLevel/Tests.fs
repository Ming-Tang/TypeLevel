namespace TypeLevel.Tests
open TypeLevel

module Test =
  let testBool1 : True = !? N3
  let testBool0 : False = !? N0

#if false
module TestList =
  open TypeLevel
  let list1 = C(N2, C(N3, C(N4, E)))
  let list2 = C(N2, C(N3, C(N4, C(N5, E))))
  let l1 = Length <|- list1
  let l2 = Length <|- list2
  let testMap = Map(Times(N2)) <|- list1
  let testFilter = Filter(PartialRight(LessEq, N3)) <|- list2

  let testFold = Fold(Add, N0) <|- list2
  let testFoldT : True = Partial(Eq, N14) <|- (Fold(Add, N0) <|- list2)
  //let testFold' = Add <|- (N2, Add <|- (N3, Add <|- (N4, Add <|- (N5, N0))))

module TestNat =
  open TypeLevel
  let p1 : Z = !-- Z
  let p2 : S<Z> = !-- (S (S Z))
  let p3 : S<S<Z>> = !++ (S Z)

  let res = (S (S Z)) -|> Pred
  let res1 = (S (S Z)) -|> Comp(Pred, Pred)
  let res2 = S (S Z) -|> Comp(Fst, Fork2(Comp(Pred, Pred), Pred))
  let res3 = S (S Z) -|> Fork2(Pred >-> Pred, Pred) >-> Fst
  let res4 = S (S (S Z)) -|> If(IsZero, Const (1 : int), Const ("" : string))
  //let res5 = (N3, N1) -|> While((Fst >-> Pred >-> IsZero >-> Not), (Fst >-> Pred) <*> Mult)

  let testRep = N3 <|*- (Times(N2), N1)
  let n15 = N0 -|> Rep(N3, Rep(N5, Succ))
  let n8' = (N3, N5) -|> Add
  let n15' = (N3, N5) -|> Mult
  let eq1 = n15 = n15'
  let n9 = (N3, N2) -|> Pow

  let testLE = N5 <<= N3
  let testLE' = N3 <<= N5
  let testEq = N3 === N5
  let testEq1 = N5 === N3
  let testEq2 = N5 === N5
  //let testWhile = N5 -|> While(IsZero >-> Not, Pred)
  let testDivRem =
    //(N8, N2)
    //-|> While(Fst >-> IsZero >-> Not, Pred <&> Succ)
    (N4, N2)
    //-|> ((Id <*> Const N0)
    //     >-> While(Fst >-> Fst >-> IsZero >-> Not,
    //               (Sub <*> Snd) <&> Succ))

  let two = N5 - N3
  let zero = N3 - N5

  //let testFactorial =
  //  N2
  //  -|> (
  //    (Id <*> (Id <*> Const N1))
  //    >-> Repeat((Fst >-> Pred) <*> Mult)
  //    >-> Snd)

  //let testFactorial1 = N4 <|*- ((Fst >-> Pred) <*> Mult, (N4, N1))
  let testX = N8 <|*- ((Fst >-> Pred) <*> Add, (N8, N1))
#endif

module TestTape =
  let singleton = Tape(E, Z, E)

  let shlSingleton = !<<< singleton
  let shrSingleton = !>>> singleton

  let tape1 = Tape(C(N2, C(N1, C(Z, E))), N3, C(N4, C(N5, C(N6, E))))

  let shlTape1 = !<<< tape1
  let shrTape1 = !>>> tape1

