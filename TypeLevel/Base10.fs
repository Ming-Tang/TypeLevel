namespace TypeLevel

module Base10 =
  open Base10Digits

  type N5d<'a, 'b, 'c, 'd, 'e> = N5d of 'a * 'b * 'c * 'd * 'e with
    static member Zero = N5d(D0, D0, D0, D0, D0)
    static member One = N5d(D0, D0, D0, D0, D1)

    static member inline (+.) (N5d(a5, a4, a3, a2, a1), N5d(b5, b4, b3, b2, b1)) =
      let inline (+++) a (b, c) =
        let cc, r1 = a + b
        let cc1, r2 = r1 + c
        let D0, cc2 = cc + cc1
        cc2, r2

      let c2, r1 = a1 + b1
      let c3, r2 = a2 +++ (b2, c2)
      let c4, r3 = a3 +++ (b3, c3)
      let c5, r4 = a4 +++ (b4, c4)
      let c6, r5 = a5 +++ (b5, c5)
      c6, N5d(r5, r4, r3, r2, r1)

    static member inline (+) (a : N5d<_, _, _, _, _>, b) =
      snd (a +. b)

    //static member inline (-) (a : N5d<_, _, _, _, _>, b : N5d<_, _, _, _, _>) =
    //  !!!(a + (!!! b))

    static member inline (*) (N5d(a5, a4, a3, a2, a1), b) =
      let b1 = b
      let b2 = !<<< b
      let b3 = !<<< (!<<< b)
      let b4 = !<<< (!<<< (!<<< b))
      let b5 = !<<< (!<<< (!<<< (!<<< b)))
      (a5 .* b5) + (a4 .* b4) + (a3 .* b3) + (a2 .* b2) + (a1 .* b1)

    static member inline (!<<<) (N5d(a5, a4, a3, a2, a1)) =
      N5d(a4, a3, a2, a1, D0)

    static member inline (!>>>) (N5d(a5, a4, a3, a2, a1)) =
      N5d(D0, a5, a4, a3, a2)

    static member inline (!!!) (N5d(a5, a4, a3, a2, a1)) : N5d<_, _, _, _, _> =
      N5d(!! a5, !! a4, !! a3, !! a2, !! a1)

  let seven = D3 + D4
  let testAdd = N5d(D0, D0, D4, D1, D2) + N5d(D0, D0, D3, D2, D4)
  let testAdd' = N5d(D0, D0, D3, D2, D4) + N5d(D0, D0, D4, D1, D2)
  let testComp = !!! N5d(D0, D2, D1, D8, D5)
  //let testSub = N5d(D0, D2, D3, D2, D4) - N5d(D0, D0, D4, D1, D3)

  let testAdd1 = N5d(D0, D0, D3, D4, D2) + (N5d(D0, D0, D3, D2, D4) + N5d(D0, D0, D4, D1, D2))
  let testAdd1' = (N5d(D0, D0, D3, D4, D2) + N5d(D0, D0, D3, D2, D4)) + N5d(D0, D0, D4, D1, D2)

  let a = N5d(D0, D0, D0, D1, D4)
  let b = N5d(D0, D0, D0, D2, D3)
  let c = N5d(D0, D0, D0, D2, D1)

  let testMult = a * b
  let testMult' = b * a
  let testMultShift = (N5d(D0, D0, D0, D0, D2) * a) * N5d(D0, D0, D1, D0, D0)
  let testMultShift' = N5d(D0, D0, D0, D0, D2) * (a * N5d(D0, D0, D1, D0, D0))
  let testMultA = (a * b) * c
  let testMultA' = a * (b * c)
  let testMult11 = N5d(D0, D0, D0, D1, D1) * N5d(D0, D0, D0, D1, D1)

