#r "nuget: FParsec"
open FParsec

type Formula<'a> = 
    | Not of Formula<'a>
    | And of Formula<'a> * Formula<'a>
    | Or of Formula<'a> * Formula<'a>
    | Imply of Formula<'a> * Formula<'a>
    | Var of 'a 

type Value<'a> =
    | Truthy of Formula<'a>
    | Falsy of Formula<'a>

type Tree<'a> =
    | Node of Value<'a> * Tree<'a> list
    | Close
    | Open

let isTruthy a =
    match a with
    | Truthy _ -> true
    | Falsy _ -> false

let rec cpc formulae path =
    match formulae with
    | Truthy a :: _ when (List.exists ((=)(Falsy a)) path) -> Close
    | Falsy a :: _ when (List.exists ((=)(Truthy a)) path) -> Close
    | Truthy (Not a) as value :: rest -> Node (value, [cpc (Falsy a :: rest) (value :: path)])
    | Truthy (And (a, b)) as value :: rest -> Node (value, [cpc (Truthy a :: Truthy b :: rest) (value :: path)])
    | Truthy (Or (a, b)) as value :: rest -> Node (value, [cpc (Truthy a :: rest) (Truthy (Or (a, b)) :: path); cpc (Truthy b :: rest) (value :: path)])
    | Truthy (Imply (a, b)) as value :: rest -> Node (value, [cpc (Falsy a :: rest) (value :: path); cpc (Truthy b :: rest) (value :: path)])
    | Truthy (Var _) as value :: rest -> Node (value, [cpc rest (value :: path)])
    | Falsy (Not a) as value :: rest -> Node (value, [cpc (Truthy a :: rest) (value :: path)])
    | Falsy (And (a, b)) as value :: rest -> Node ((value), [cpc (Falsy a :: rest) (value :: path); cpc (Falsy b :: rest) (value :: path)])
    | Falsy (Or (a, b)) as value :: rest -> Node ((value), [cpc (Falsy a :: Falsy b :: rest) (value :: path)])
    | Falsy (Imply (a, b)) as value :: rest -> Node ((value), [cpc (Truthy a :: Falsy b :: rest) (value :: path)])
    | Falsy (Var _) as value :: rest -> Node (value, [cpc rest (value :: path)])
    | _ -> Open

let rec ipc formulae path =
    match formulae with
    | Truthy a :: _ when (List.exists ((=)(Falsy a)) path) -> Close
    | Falsy a :: _ when (List.exists ((=)(Truthy a)) path) -> Close
    | Truthy (Not a) as value :: rest -> Node (value, [ipc (Falsy a :: rest) (value :: path)])
    | Truthy (And (a, b)) as value :: rest -> Node (value, [ipc (Truthy a :: Truthy b :: rest) (value :: path)])
    | Truthy (Or (a, b)) as value :: rest -> Node (value, [ipc (Truthy a :: rest) (Truthy (Or (a, b)) :: path); ipc (Truthy b :: rest) (value :: path)])
    | Truthy (Imply (a, b)) as value :: rest -> Node (value, [ipc (Falsy a :: rest) (value :: path); ipc (Truthy b :: rest) (value :: path)])
    | Truthy (Var _) as value :: rest -> Node (value, [ipc rest (value :: path)])
    | Falsy (Not a) as value :: rest -> Node (value, [ipc (Truthy a :: rest) (List.filter isTruthy (value :: path))])
    | Falsy (And (a, b)) as value :: rest -> Node ((value), [ipc (Falsy a :: rest) (value :: path); ipc (Falsy b :: rest) (value :: path)])
    | Falsy (Or (a, b)) as value :: rest -> Node ((value), [ipc (Falsy a :: Falsy b :: rest) (value :: path)])
    | Falsy (Imply (a, b)) as value :: rest -> Node ((value), [ipc (Truthy a :: Falsy b :: rest) (List.filter isTruthy (value :: path))])
    | Falsy (Var _) as value :: rest -> Node (value, [ipc rest (value :: path)])
    | _ -> Open

let rec isClosed tree =
    match tree with
    | Node (_, cs) -> List.forall isClosed cs
    | Close -> true
    | Open -> false

let isProbable r a c = r (Falsy c :: List.map Truthy a) [] |> isClosed

let pFormula, pFormulaRef = createParserForwardedToRef()

let pFormulae = many (pFormula .>> spaces)

let pAnd = parse {
    do! skipString "and"
    do! spaces1
    let! a = pFormula
    do! spaces1
    let! b = pFormula
    return And (a, b)
}

let pOr = parse {
    do! skipString "or"
    do! spaces1
    let! a = pFormula
    do! spaces1
    let! b = pFormula
    return Or (a, b)
}

let pImply = parse {
    do! skipString "imply"
    do! spaces1
    let! a = pFormula
    do! spaces1
    let! b = pFormula
    return Imply (a, b)
}

let pNot = parse {
    do! skipString "not"
    do! spaces1
    let! a = pFormula
    return Not a
}

let pVar = parse {
    let! a = letter
    return Var a
}

pFormulaRef := pVar <|> between (pchar '(') (pchar ')') (choice [pNot; pAnd; pOr; pImply])

let testp calc conc assm =
    match ((run pFormula conc), (run pFormulae assm)) with
    | (Success (conclusion, _, _), Success (assumptions, _, _)) ->
        if (isProbable calc assumptions conclusion) then
            printfn "probable"
        else
            printfn "unprobable"
    | (Failure (msg, _, _), _) | (_, Failure (msg, _, _)) ->
        printfn "Failure: %s" msg

testp cpc "p" "(not (not p))"