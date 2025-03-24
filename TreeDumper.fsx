// dump the directory structure that exists from cwd and down. ignoring most files

open System

let indentAmount = 2
let dirIndent = "-"
let fileIndent = "+"
let interestingFiles = [ ".xml"; ".nupkg" ]

let blacklist = [ "bin"; "obj"; "packages"; "runtimes"; ".git" ]

module Helpers =

    let (|ValueString|_|) x =
        if String.IsNullOrWhiteSpace x then None else Some x

    let isValueString =
        function
        | ValueString _ -> true
        | _ -> false

    let afterOpt (delim: string) (x: string) =
        if not <| isValueString delim then
            failwith "Delimiter must not be null or empty"

        match x.IndexOf delim with
        | i when i < 0 -> None
        | i -> Some x.[i + delim.Length ..]

    let afterOrSelf delim x =
        afterOpt delim x |> Option.defaultValue x

    let getFileCount d =
        IO.Directory.GetFiles d |> Seq.filter (fun f -> IO.File.Exists f) |> Seq.length

open Helpers

module Option =
    // swallow the error, make an option
    let ofOk =
        function
        | Ok x -> Some x
        | _ -> None

let tryResult f x =
    try
        f x |> Ok
    with ex ->
        Error ex

let tryResult' f = tryResult f ()


let enumerateFilesByExtension (extensions: string seq) (dir: string) : string seq =
    System.IO.Directory.EnumerateFiles(dir, "*.*")
    |> Seq.filter (fun x -> extensions |> Seq.exists x.EndsWith)

// assume something ending in .zip or .nupkg
let searchForPackage (dir: string) =
    // System.IO.Directory.EnumerateFiles(dir, "*.xml|*.nupkg", System.IO.SearchOption.AllDirectories)
    enumerateFilesByExtension interestingFiles dir

let containsBlacklistItem (path: string) =
    blacklist |> Seq.exists (fun bl -> path.Contains bl) // not accounting for case insensitivity

let trimEnclosing (enclosure: string) (path: string) = path |> afterOrSelf enclosure

type PathResult = Directory of string * (string list) * Result<int, exn>

// initial call will not catch if it fails, child calls however will be
let rec walkDirectories depth (path: string) = [
    for d in System.IO.Directory.EnumerateDirectories path do
        let goForWalkies () = walkDirectories (depth + 1) d
        // simple blacklisting does not account for hybrid words or path segmenting to properly respect the blacklist
        if containsBlacklistItem d |> not then
            let files: string list =
                try
                    searchForPackage d
                    |> Seq.filter (containsBlacklistItem >> not)
                    |> Seq.map (trimEnclosing d)
                    |> List.ofSeq
                with ex ->
                    eprintfn "Could not enumerate packages in '%s'" d
                    List.empty

            yield (depth, PathResult.Directory(d, files, d |> tryResult getFileCount))

            match tryResult' goForWalkies with
            | Ok items -> yield! items
            | Error ex -> printfn $"Failed to descend into '%s{d}': '%s{ex.Message}'"
]

printfn "Starting in '%s'" Environment.CurrentDirectory

if IO.Directory.Exists Environment.CurrentDirectory |> not then
    eprintfn "Current directory doesn't exist"

let banner = "-------------------------"
let printBanner () = printfn "%s" banner
printBanner ()

walkDirectories 0 Environment.CurrentDirectory
|> Seq.iter (fun (depth, pr) ->
    let writeInfo indent p extra = printfn $"%s{indent}%s{p}%s{extra}"

    match pr with
    | PathResult.Directory(dir, files, countOpt) ->
        let rep = String.replicate ((depth + 1) * indentAmount)
        let indent = rep dirIndent

        let c =
            countOpt
            |> Option.ofOk
            |> Option.map string
            |> Option.defaultValue "?"
            |> sprintf "(%s)"

        writeInfo indent (sprintf "%s%c" dir IO.Path.DirectorySeparatorChar) c
        // writeInfo indent (sprintf "%s%c" dir '&') c

        files
        |> Seq.iter (fun f ->
            let indent = rep fileIndent

            let f =
                if f.StartsWith IO.Path.DirectorySeparatorChar then
                    f[1..]
                else
                    f

            writeInfo indent f null)

        if depth = 0 then // for each root-level directory lets footer it
            printBanner ()

)
