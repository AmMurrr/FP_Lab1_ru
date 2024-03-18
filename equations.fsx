open System

let eps = 0.000001


    // Определите функции для решение алгебраических уравнений

let rec dichotomy f a b =
   let c = (a + b) / 2.0
   if abs (b - a) < eps then c
   elif f a * f c < 0.0 then dichotomy f a c
   else dichotomy f c b

let rec iterations phi x0 = 
  let x1 = phi x0
  if abs ( x1 - x0) < eps then x1
  else iterations phi x1 

let newthon f f' x0 = 
    iterations (fun x -> x - ((f x) / (f' x))) x0
    // используйте функцию 'iterations'

    // Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x= x + sqrt x + x**(1.0/3.0) - 2.5
let f2 x= x - (1./(3. + sin (3.6*x) ))
let f3 x= 0.1*x**2.0 - x* log x

let f1' x= (3.0 * (x**(2.0 / 3.0) + 2.0 * sqrt x ) )/ (6.0 * x ** (7.0 / 6.0)) + 1.0 
let f2' x= (18. * cos ((18. *x)/5. ) )/(5. *( sin ((18.*x)/5.)**2.+ 6.*sin ((18.*x)/5.) + 9.))+1.
let f3' x= (-1. * log x )+x/5.0 - 1. 

let phi1 x=  -1.0 * sqrt x - x ** (1.0 / 3.0) + 2.5 
let phi2 x= (1./(3. + sin (3.6*x) ))
let phi3 x= Math.E ** (0.1 * x) // Math.E is a const, so i think it`s ok to use it? You can use 2,7182 with the same result.

let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1  0.4 1.) (iterations phi1 0.7) (newthon f1 f1' 0.4)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2  0. 0.85) (iterations phi2 0.425) (newthon f2 f2' 0.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3  1. 2.) (iterations phi3 1.5) (newthon f3 f3' 1.)
