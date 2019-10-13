open System
//Méthode globale
let charToInt c = int c - int '0'

let rec texte ((x:int),(pos:int),(resultat:int))=
    let mutable y:int = resultat
    let f:int = ((char)(x.ToString().Substring(pos,1))|>charToInt)
    y <- y + (pown f (x.ToString().Length))
    if pos = (x.ToString().Length-1) then 
        y
    else 
        texte(x,pos+1,y)

let Exercice1 x = 
    let long = x.ToString().Length
    let mutable y = 0
    y <- texte(x,0,0)
    if y = x && x > 0 then "C'est un chiffre narcissique"
    else "Ce n'est pas un chiffre narcissique"

let Exercice2 x = 
    let mutable puiss = 0
    let mutable negatif = false
    if x < 0 then
         puiss <- (x.ToString().Length-2)
         negatif <- true
    elif x > 0 then
         puiss <- (x.ToString().Length-1)
    let mutable y:string =""
    for i in x.ToString() do
        let mutable f:int = i|>charToInt 
        if f <> -3 then
            if f > 0 && negatif=true then 
                f <- f - (f*2)
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


let Exercice3 ((h:int),(m:int),(p:string))((h1:int),(m1:int),(p1:string)) = 
    let mutable mauvaiseEntrée = false
    let mutable resultat = ""
    let mutable h = h
    let mutable h1 = h1
    if m >= 60 || h >= 24 || (p <> "AM" && p <> "PM")  then
        resultat <- "mauvaise entrée"
        mauvaiseEntrée <- true
    elif m1 >= 60 || h1 >= 24 || (p1 <> "AM" && p1 <> "PM")  then
        resultat <- "mauvaise entrée"
        mauvaiseEntrée <- true
    elif (h >= 12 && p = "AM") && (h1 >= 12 && p = "AM") then
        resultat <- "mauvaise entrée"
        mauvaiseEntrée <- true
    if p = "PM" && h < 12 then
        h <- h+12
    if p1 = "PM" && h1 < 12 then
        h1 <- h1+12
    if mauvaiseEntrée = false then
        if p = "PM" && p1 = "AM" then
            resultat <- h1.ToString()+"h"+ m1.ToString()+ " "+ p1 
        elif p1 = "PM" && p = "AM" then
            resultat <- h.ToString()+"h"+ m.ToString()+ " "+ p
        elif (p = "AM" && p1 = "AM")||(p = "PM" && p1 = "PM")  then
            if h > h1 then
                resultat <- h1.ToString()+"h"+ m1.ToString()+ " "+ p1
            elif h1 > h then
                resultat <- h.ToString()+"h"+ m.ToString()+ " "+ p
            elif m > m1 then
                resultat <- h1.ToString()+"h"+ m1.ToString()+ " "+ p1
            elif m1 > m then
                resultat <- h.ToString()+"h"+ m.ToString()+ " "+ p
        elif p = p1 && h = h1 && m = m1 then
             resultat <- h.ToString()+"h"+ m.ToString()+ " "+ p
    resultat


let conditionSucces (nom:String,exam1:float,exam2:float,tp1:float,tp2:float,tp3:float) = 
    if ((exam1 + exam2)/2.0 < 60.0 || (tp1+tp2+tp3)/3.0 < 60.0 || ((exam1 + exam2)/2.0 + (tp1+tp2+tp3)/3.0) < 60.0) then
        false
    else
        true

let rec nomPlusMoyenne list retour (pos:int) = 
    let mutable moyenne = 0.0
    if pos < List.length(list) then
        let nom,exam1,exam2,tp1,tp2,tp3 = list.[pos]
        moyenne <- ((exam1*0.2)+(exam2*0.35)+(tp1*0.2)+(tp2*0.15)+(tp3*0.1))
        let tuple = (nom,moyenne)
        nomPlusMoyenne (list) (tuple::retour) (pos+1)
    else
        retour

let note tuple = 
    let nom,note = tuple
    note

let Exercice4 list = 
    //Afficher pour chaque étudiant sa moyenne sachant les pondérations suivantes
    let mutable moyenneEtudiant = 0.0
    let mutable moyenneClasse = 0.0
    let mutable NombreEleve = 0
    let mutable Variance = 0.0
    let mutable EcartType = 0.0

    for i in list do
        let nom,exam1,exam2,tp1,tp2,tp3 = i
        moyenneEtudiant <- ((exam1*0.2)+(exam2*0.35)+(tp1*0.2)+(tp2*0.15)+(tp3*0.1))
        moyenneClasse <- moyenneClasse + moyenneEtudiant
        printfn "Nom %s, Note moyenne: %s" nom (moyenneEtudiant.ToString("0.00")+"%")
        NombreEleve <- NombreEleve+1


    printfn "Moyenne de la classe: %s" ((moyenneClasse/(float)NombreEleve).ToString("0.00")+"%")


    for i in list do
        let nom,exam1,exam2,tp1,tp2,tp3 = i
        moyenneEtudiant <- ((exam1*0.2)+(exam2*0.35)+(tp1*0.2)+(tp2*0.15)+(tp3*0.1))
        Variance <- Variance + ((moyenneEtudiant - moyenneClasse)**2.0)
    Variance <- Variance/((float)NombreEleve)
    EcartType <- sqrt(Variance)
    printfn "Écart-type: %f" EcartType

    
    let liste1,liste2 = List.partition(fun elem -> conditionSucces(elem) = true)list
    printfn "Succès : %A \nÉchec: %A" liste1 liste2 

    let NomMoyenne = (nomPlusMoyenne list [] 0)
    let trier = List.sortByDescending(fun elem -> note(elem))NomMoyenne
    printfn "Liste moyenne trier:  %A" trier

    let mutable nomE,exam1E,exam2E,tp1E,tp2E,tp3E = ("",0.0,0.0,0.0,0.0,0.0)
    for i in list do    
        let nom,exam1,exam2,tp1,tp2,tp3 = i
        if ((exam1 - exam1E)+(exam2 - exam2E)+(tp1 - tp1E)+(tp2 - tp2E)+(tp3 - tp3E)) > 0.0 then
            nomE<-nom
            exam1E<-exam1
            exam2E<-exam2
            tp1E<-tp1
            tp2E<-tp2
            tp3E<-tp3
    printfn "L'étudiant récipiant d'or est %s avec exam1: %s , exam2: %s, tp1: %s, tp2: %s, tp3: %s" nomE (exam1E.ToString("0.00")+"%") (exam2E.ToString("0.00")+"%") (tp1E.ToString("0.00")+"%") (tp2E.ToString("0.00")+"%") (tp3E.ToString("0.00")+"%")

[<EntryPoint>]
let main argv =
    Console.WriteLine("Exercice #1 : "+Exercice1 1634)
    Console.WriteLine("Exercice #2 : "+Exercice2 -1305)
    printfn "Exercice3: %s" (Exercice3(0,20,"AM")(11,23,"PM"))
    Exercice4([("samuel",60.0,50.0,80.0,90.0,70.0);("antoine",65.0,100.0,60.0,92.0,65.0);("test",80.0,80.0,80.0,90.0,90.0)])
    Console.ReadKey()|>ignore;
    0 // return an integer exit code
