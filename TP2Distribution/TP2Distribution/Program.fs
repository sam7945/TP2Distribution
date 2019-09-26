open System

open System
//Méthode globale
let charToInt c = int c - int '0'



//x= nombre init,
let Exercice1 x = 
    let long = x.ToString().Length
    let mutable y = 0
    for i in x.ToString() do
        let f:int = i|>charToInt
        y <- y + (pown f long)
    if y = x && x > 0 then "C'est un chiffre narcissique"
    else "Ce n'est pas un chiffre narcissique"
      


[<EntryPoint>]
let main argv =
    Console.WriteLine("Exercice #1 : "+Exercice1 0)
    Console.ReadKey()|>ignore;
    0 // return an integer exit code
