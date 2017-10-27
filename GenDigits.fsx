

let b = 10
for i = 0 to b-1 do
  printfn "  type D%d with" i
  for j = 0 to b-1 do
    let k = i + j
    printfn "    static member inline (+) (D%d, D%d) = (D%d, D%d)" i j (k / b) (k % b)
  printfn ""

