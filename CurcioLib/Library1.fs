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

[<ExcelFunction(Description="Return antigens that are in the minuend but not in subtrahend")>]
let AntigenDifference (minuend:string) (subtrahend:string) = 
    let antigen_minuend, values_minuend = parse_antigen_string minuend
    let antigen_subtrahend, values_subtrahend = parse_antigen_string subtrahend
    let antigen_minuend, antigen_subtrahend = Set.ofSeq antigen_minuend, Set.ofSeq antigen_subtrahend
    let antigen_difference = Set.difference antigen_minuend antigen_subtrahend |> Set.toSeq
    System.String.Join(" ",  antigen_difference) 

[<ExcelFunction(Description="Return sum of antigen values in input string")>]
let AntigenValueSum (input:string)  = 
    let antigen_input, values_input = parse_antigen_string input
    Seq.map (fun (a:string) -> System.Int32.Parse(a)) values_input
        |> Seq.fold (fun (acc:int) (a:int) -> acc + a) 0 
