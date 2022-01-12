module CurcioLib

open ExcelDna.Integration

let parse_antigen_string (input:string) = 
    let mutable antigens = List.empty
    let mutable values = List.empty
    let temp = input.Split(' ')
    for t in temp do
        let tt = t.Split('(',')')
        antigens <- tt.[0] :: antigens
        values <- tt.[1] :: values
    done
    antigens, values

let parse_antigen_string_on_hla_type (input:string) (hla_type:string) = 
    let mutable antigens = List.empty
    let mutable values = List.empty
    let temp = input.Split(' ')
    for t in temp do
        let tt = t.Split('(',')')
        if tt.[0].Contains(hla_type)
            then
                antigens <- tt.[0] :: antigens
                values <- tt.[1] :: values
    done
    antigens, values

let parse_antigen_parts_string (input:string) = 
    let mutable antigens = List.empty
    let temp = input.Split(' ')
    for t in temp do
        let tt = t.Split('(',')')
        antigens <- (tt.[0],tt.[1]) :: antigens
    done
    antigens

[<ExcelFunction(Description="Return antigens that are in the minuend but not in subtrahend")>]
let AntigenDifference (minuend:string) (subtrahend:string) = 
    try
        let antigen_minuend, _ = parse_antigen_string minuend
        let antigen_subtrahend, _ = parse_antigen_string subtrahend
        let antigen_minuend, antigen_subtrahend = Set.ofSeq antigen_minuend, Set.ofSeq antigen_subtrahend
        let antigen_difference = Set.difference antigen_minuend antigen_subtrahend |> Set.toSeq
        System.String.Join(" ",  antigen_difference) 
    with ex ->
        ""

[<ExcelFunction(Description="Return antigens that are in the minuend but not in subtrahend considering only an HLT type")>]
let AntigenDifferenceOnHlaType (minuend:string) (subtrahend:string) (hla_type:string) = 
    try
        let antigen_minuend, _ = parse_antigen_string_on_hla_type minuend hla_type
        let antigen_subtrahend, _ = parse_antigen_string_on_hla_type subtrahend hla_type
        let antigen_minuend, antigen_subtrahend = Set.ofSeq antigen_minuend, Set.ofSeq antigen_subtrahend
        let antigen_difference = Set.difference antigen_minuend antigen_subtrahend |> Set.toSeq
        System.String.Join(" ",  antigen_difference) 
    with ex ->
        ""
    

[<ExcelFunction(Description="Return sum of antigen values in input string considering only an HLT type")>]
let AntigenValueSum (input:string, hla_type:string)  =
    try
        let antigen_input = parse_antigen_parts_string input
        antigen_input
            |> Seq.filter (fun (t:string, v:string) -> t.Contains(hla_type)) 
            |> Seq.map (fun (t, v:string) -> System.Int32.Parse(v)) 
            |> Seq.fold (fun (acc:int) (a:int) -> acc + a) 0 
    with ex ->
        0
