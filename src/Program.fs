open System

type DiceCode =
    {
      Articles: int
      Eyes: int
    }
    
let tryParseInt (s: string): int option =
    match Int32.TryParse s with
    | (true, value) -> Some(value)
    | (false, _) -> None

let tryAllParseInt (arr: string array): option<int * int> =
    match arr with
    | [|a; e|] -> 
        let articles = tryParseInt a
        let eyes = tryParseInt e
        match (articles, eyes) with
        | (Some(articles), Some(eyes)) -> Some(articles, eyes)
        | (_, _) -> None
    | _ -> None

let parseDiceCode (s: string): DiceCode option =
    match s.Split('d', 'D') |> tryAllParseInt with
    | Some(a, e) -> {Articles = a; Eyes = e} |> Some
    | None -> None

let diceRoll (d: DiceCode): int = 
    let random = Random()
    Seq.reduce (fun p c -> p + c ) (seq { for i in 1..d.Articles -> random.Next(1,d.Eyes + 1) })


[<EntryPoint>]
let main argv =
    match let arg = if argv.Length = 1 then argv.[0] else String.Empty in parseDiceCode arg with
    | Some(d) -> d |> diceRoll |> printfn "%A" |> fun _ -> 0
    | None -> stderr.WriteLine "use: dice [Number][d or D][Number of eyes] (ex: 2d6)" |> fun _ -> 1
    