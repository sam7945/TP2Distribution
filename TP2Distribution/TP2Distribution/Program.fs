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

let Exercice2 x = 
    let mutable puiss = x.ToString().Length-1
    let mutable y:string =""
    for i in x.ToString() do
        let f:int = i|>charToInt
        if f <> 0 && puiss >= 1 then 
            y <- y + f.ToString()+"*10^"+puiss.ToString() + "+"
            puiss <- puiss - 1
        elif f <> 0 && puiss = 0 then
            y <- y + f.ToString()
            puiss <- puiss - 1
        elif f = 0 then 
            puiss <- puiss - 1
    if y.EndsWith("+") then y <- y.Substring(0,y.Length-1)
    y   


[<EntryPoint>]
let main argv =
    Console.WriteLine("Exercice #1 : "+Exercice1 0)
    Console.WriteLine("Exercice #2 : "+Exercice2 205)
    Console.ReadKey()|>ignore;
    0 // return an integer exit code
