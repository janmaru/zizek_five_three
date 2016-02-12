type Male = 
    | White
    | Black
  
let rec choices = function
    | []      -> []
    | p::tail -> (p,tail) :: [ for (y,l) in choices tail -> (y,l) ]

let Men = [White; White; White; Black; Black]

printfn "%A" (choices  Men)

//  [(White, [White; White; Black; Black]); (White, [White; Black; Black]);
//  (White, [Black; Black]); (Black, [Black]); (Black, [])]

let rec combinations S k =
    [ if k=0 then yield [] else
            for (e,r) in choices S do
                for o in combinations r (k-1) do yield e::o  ]

type Position =
    | A
    | B
    | C

let Positions = [A;B;C]

printfn "%A" (combinations Men Positions.Length)
    //[[White; White; White]; [White; White; Black]; [White; White; Black];
    //[White; White; Black]; [White; White; Black]; [White; Black; Black];
    //[White; White; Black]; [White; White; Black]; [White; Black; Black];
    //[White; Black; Black]]

//List.distinct returns a list that contains no duplicate entries according to generic hash 
//and equality comparisons on the entries. If an element occurs multiple times in the list 
//then the later occurrences are discarded.

let Couples = (combinations Men Positions.Length) |> List.distinct

printfn "%A" (Couples)
//  [[White; White; White]; [White; White; Black]; [White; Black; Black]]

let removeAt index lista =
    lista 
    // Associate each element with a boolean flag specifying whether 
    // we want to keep the element in the resulting list
    |> List.mapi (fun i el -> (i <> index, el)) //index from (0)
    // Remove elements for which the flag is 'false' and drop the flags
    |> List.filter fst |> List.map snd
 

//for dance in Couples do
//    let counter = ref 0
//    for m in dance do
//        printfn "%A" (removeAt !counter dance , Positions.[!counter])
//        counter := !counter + 1 
//([White; White], A)
//([White; White], B)
//([White; White], C)

//([White; Black], A)
//([White; Black], B)
//([White; White], C)

//([Black; Black], A)
//([White; Black], B)
//([White; Black], C)
let mutable matches = []
for dance in Couples do
    let counter = ref 0
    for m in dance do
        matches <- (removeAt !counter dance , Positions.[!counter])::matches 
        counter := !counter + 1

//([White; Black], C)
//([White; Black], B)
//([Black; Black], A)
//([White; White], C)
//([White; Black], B)
//([White; Black], A)
//([White; White], C)
//([White; White], B)
//([White; White], A)
    
for c in matches do
    printfn "%A" c

let free w = match w with 
             | [White; White] -> false 
             | [White; Black] -> false 
             | [Black; Black] -> true
             | _-> false


for c in matches do
    printfn "%A" (free (fst c))


System.Console.ReadKey(true)|>ignore