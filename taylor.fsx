open System

// Print a table of a given function f, computed by taylor series

// function to compute
let f x = ((1.0 + x ** 2.0) / 2.0 ) * (atan x) - (x / 2.0)


let a = 0.1
let b = 0.6
let n = 10
let eps = 1e-7


// Define a function to compute f using naive taylor series method
let taylor_naive x eps =
  let rec taylorSum acc i x eps=
        let term = ( (-1.0)**(i + 1.0))*((x**(2.* i + 1.))/(4.* i ** 2. - 1.))
        if abs term < eps then (acc, i)
        else taylorSum (acc + term) (i + 1.) x eps
  taylorSum 0.0 0. x eps
    


// Define a function to do the same in a more efficient way
let taylor x = 
    let new_el i prev = prev * (((-1.)**(2. * i + 1.)) * ((x**2. * (4. * i**2. - 1. - 8. * i + 4.)) / (4. * i**2 - 1.))  )
    let rec iter i pr acc =
        let el = new_el i pr
        if abs el < eps
        then (acc,i)
        else iter (i + 1.0) el (acc + pr)
    iter 0.0 x 0.0

let main =
    printfn " x \t builtin\t naive \t terms \t smart  \t terms"
    for j=0 to n do
        let x = a+(float j)/(float n)*(b-a)
        let naive_t = taylor_naive x eps
        let smart_t = taylor x
        printfn "%5.2f  %10.6f   %10.6f   %3.1f   %10.6f  %3.1f" x (f x) (fst naive_t) (snd naive_t) (fst smart_t) (snd smart_t)

main
