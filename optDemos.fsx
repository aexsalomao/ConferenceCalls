#r "nuget: DiffSharp-lite, 1.0.0-preview-987646120"

open DiffSharp
open DiffSharp.Optim

let f (x: Tensor) = 

    let x, y = x.[0], x.[1]

    (3. * x ** (3.0)) + 2.0 * y ** 2.0

let lr, momentum, iters, threshold = 1e-3, 0.5, 1000, 1e-3

let guess = dsharp.tensor([1.; 0.01])

let fx, x = optim.sgd(f, guess, lr=dsharp.tensor(lr), momentum=dsharp.tensor(momentum), nesterov=true, iters=iters, threshold=threshold)


#r "nuget: Flips, 2.4.5"
open Flips

