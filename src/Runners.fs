//cspell: ignore octopusdeploy
module TfParse.Runners

open FParsec

open BReuse

open TfParse.Parsers

let getErrors (erm: ErrorMessageList) =
    if isNull erm then
        None
    else
        erm
        |> Seq.unfold (fun eml ->
            if isNull eml || isNull eml.Head then
                None
            else
                Some(eml.Head, eml.Tail))
        |> Some

let runOnString name p input =
    runParserOnString p { ParserStack = List.empty } name input

let inline createRunHarness parser =
    fun (input: string) ->
        match runOnString "" parser input with
        | Success(x, _, _) -> sprintf "Parsed: %A ('%s')" x (String.truncate 10 input) |> Result.Ok
        | Failure(errMsg, pe, ps) -> Result.Error(sprintf "Parsing failed: %s" errMsg, string ps.ParserStack)

let runSettingParser indent (input: string) =
    match runOnString "" unquotedSetting input with
    | Success((k, v), _, _) -> sprintf "Parsed Setting: %s = %A" k v |> Result.Ok
    | Failure(errMsg, _, _) -> Result.Error(sprintf "%sParsing failed: %s" indent errMsg, "S Parser")

let runIdentifierParser input = createRunHarness identifier input

let runUnquotedSParser (input: string) = createRunHarness unquotedSetting input

let runTObjectBodyParser input = createRunHarness tObjectBody input

let runTObjectParser input = createRunHarness tObject input

// Helper function to run the parser
let runParser (input: string) =
    match runOnString "" moduleParser input with
    | Success(result, _, _) ->
        printfn "Parsed module: Source: %s, Version: %s" result.Source (result.Version |> Option.defaultValue "latest")
        printfn "Settings:"
        result.Settings |> List.iter (fun s -> printfn "\t%A" s)
    | Failure(errMsg, pe, us) ->
        eprintfn "Parsing failed: %s" errMsg
        eprintfn "Msgs:"

        pe.Messages
        |> getErrors
        |> Option.iter (Seq.iter (fun em -> eprintfn "\t%A:%A" (em.GetType().Name) em.Type))

        eprintfn "PE: %A" pe
        failwithf "Parser input: '%s'" input

// type AnnounceSettings = {
//     AnnounceCategories: bool
//     AnnounceSuccess: bool
// }

let runValidationHarness (parser: MyParser<'t>) (fts: Samples.FullTestSample<'t>) : Result<string, string * string> =

    let checkValidation pos userState state (validator: Samples.ValidatorType) : Result<'t, string * string> =
        match state with
        | Result.Ok value ->
            match validator with
            | Samples.ValidatorType.PositionExpectation f ->
                if f pos then
                    Result.Ok value
                else
                    Result.Error($"Position unexpected:Index:%i{pos.Index},%A{pos},US:%A{userState}", string value)

        | result -> result

    match runOnString "" parser fts.Input, fts.Expected with
    | Success(x: 't, _, pos), Result.Error() -> Result.Error($"Expected failure, finished at %A{pos}", string x)
    | Failure(errMsg, pe, us), Result.Ok v ->
        Result.Error($"Expected success at %i{pe.Position.Index}: %s{errMsg}", "US:" + string us.ParserStack)
    | Failure(_, _, _), Result.Error() -> Result.Ok "Parser Failed Successfully"

    | Success(x: 't, us, pos), Result.Ok expectedOpt ->
        let state: Result<'t, string * string> =
            match expectedOpt with
            | None -> Result.Ok x
            | Some(expected: 't) ->
                if expected = x then
                    Result.Ok x
                else
                    Result.Error($"Parser succeeded but expected: %A{expected}", $"US:%A{us.ParserStack};%A{x}")

        (state, fts.Validators)
        ||> Seq.fold (checkValidation pos us)
        |> function
            | Result.Ok _ -> sprintf "Parsed: %A ('%s')" x (String.truncate 10 fts.Input) |> Result.Ok
            | Result.Error(m1, m2) -> Result.Error(m1, m2)

let runSamples announceCategories =
    let runList parser items : Result<string, string * string> list =
        items
        |> List.map (fun item ->
            // printfn "%s%i:" indent i

            try
                parser item
            // Result.Ok($"{state.Result}")
            with ex ->
                Result.Error(ex.Message, ex.Message))

    let i = "\t"


    // for samples where the length should be the total length

    [
        // "Space", Samples.wsSamples |> List.map (createRunHarness ws)
        "Space", Samples.wsSamples |> List.map (runValidationHarness ws)
        "Int", Samples.intSamples |> List.map (runValidationHarness integer)
        "IntDotInt", Samples.idiSamples |> List.map (runValidationHarness intDotIntParser)
        "Value", Samples.valuesSimple |> List.map (createRunHarness settingValueParser)

        "Comment", Samples.commentSamples |> List.map (createRunHarness singleLineComment)

        "Version", Samples.simpleVersion |> runList (runValidationHarness versionParser)

        "CsvItem",
        Samples.csvItems
        |> runList (runValidationHarness (csvItemParser settingValueParser))
        "CsvTuple",
        Samples.csvTuple
        |> runList (runValidationHarness (csvItemParser integer .>>. csvItemParser integer))

        "SimpleArray",
        Samples.arrayExamples
        |> List.map (createRunHarness (arrayParser settingValueParser))

        "Csv",
        Samples.csvExamples
        |> runList (runValidationHarness (csvParser settingValueParser))




        "LEqR",
        Samples.lEqRSamples
        |> List.map (runValidationHarness (lEqR identifier identifier))

        "Identifier", Samples.identifierSamples |> List.map (runValidationHarness identifier)


        "UnquotedSetting", Samples.unquotedSettingSamples |> runList runUnquotedSParser
        "Setting",
        [
            "version = \"1.0.0\""
            "enabled = true"
            "octopus = octopus "
            "octopus=octopusdeploy"
        ]
        |> List.map (runSettingParser i)

        "tObjectBody", Samples.tObjectBodySamples |> runList runTObjectBodyParser
        "tObject", Samples.tObjectSamples |> List.map runTObjectParser

        "description", Samples.exampleDescription |> List.map (createRunHarness descriptionBlockParser)

        "SettingBlock", Samples.exampleSettingBlocks |> runList (createRunHarness settingBlockParser)

        "csv quoted list",
        Samples.quotedStringItems
        |> runList (createRunHarness (ws >>. csvParser quotedString))

        "QuotedString List", Samples.quotedStringList |> runList (createRunHarness qsListParser)

        "JsonEncodeKvp", Samples.jsonEncodeKvp |> runList (createRunHarness JsonEncode.keyValuePair)

        "JsonEncodeBody", Samples.jsonObjects |> runList (runValidationHarness JsonEncode.jsonEncodeBody)
        "JsonEncode",
        Samples.jsonEncodeExamples
        |> runList (createRunHarness JsonEncode.jsonencodeParser)

        "ProjectEnvs", Samples.projectEnv |> runList (createRunHarness projectEnvParser)

        "Project", Samples.exampleProj |> runList (createRunHarness projectParser)

        "ProjectList", Samples.exampleProjectList |> runList (createRunHarness projectListParser)

        "exampleAttachedProjectList",
        Samples.exampleAttachedProjectList
        |> runList (createRunHarness attachedProjectsParser)

        "Ms", Samples.exampleMs |> runList (createRunHarness mParser)
    ]
    |> List.iter (fun (title, results) ->
        let successMsg = sprintf "%s%c %s" i check title

        let hasError =
            results
            |> Seq.exists (function
                | Result.Error _ -> true
                | _ -> false)

        if hasError then
            eprintfn "%s parser failed:" title

            results
            |> List.iteri (fun index ->
                function
                | Result.Ok msg -> printfn "%s %i %s" successMsg index msg
                | Result.Error(e, f) ->
                    eprintfn "%s%c %i %s" i xMark index e

                    // consider indenting this value if it is multi-line
                    if String.isValueString f then
                        eprintfn "%s" f

                    printfn "^^^^^^^^^^^^"
                    printfn ""

            )


            // printfn "%A" results
            failwithf "%s failed" title
        elif announceCategories then
            printfn "%s(%i tests)" successMsg results.Length
    // else
    //     printfn "%s finished" title
    )



let runParserMainSample announceCategories =

    runSamples announceCategories

    printfn "Starting main sample"

    runParser Samples.exampleInput // |> Dump |> ignore
