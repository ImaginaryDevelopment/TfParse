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

let inline createRunHarness parser =
    fun (input: string) ->
        match run parser input with
        | Success(x, _, _) -> sprintf "Parsed: %A ('%s')" x (String.truncate 10 input) |> Result.Ok
        | Failure(errMsg, _, _) -> Result.Error(sprintf "Parsing failed: %s" errMsg, "")

let runIdentifierParser input = createRunHarness identifier input

let runUnquotedSParser (input: string) = createRunHarness unquotedSetting input

let runTObjectBodyParser input = createRunHarness tObjectBody input

let runTObjectParser input = createRunHarness tObject input

let runSettingParser indent (input: string) =
    match run unquotedSetting input with
    | Success((k, v), _, _) -> sprintf "Parsed Setting: %s = %A" k v |> Result.Ok
    | Failure(errMsg, _, _) -> Result.Error(sprintf "%sParsing failed: %s" indent errMsg, "S Parser")

// Helper function to run the parser
let runParser (input: string) =
    match run moduleParser input with
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

let runSamples announceCategories =
    let runList parser items : Result<string, string * string> list =
        items
        |> List.mapi (fun i item ->
            // printfn "%s%i:" indent i

            try
                parser item
            // Result.Ok($"{state.Result}")
            with ex ->
                Result.Error(ex.Message, ex.Message))

    let i = "\t"

    [

        "Space", Samples.wsSamples |> List.map (createRunHarness ws)
        "Value", Samples.valuesSimple |> List.map (createRunHarness simpleValueParser)

        "Comment", Samples.commentSamples |> List.map (createRunHarness singleLineComment)

        "Version", Samples.simpleVersion |> runList (createRunHarness versionParser)

        "Csv", Samples.csvExamples |> List.map (createRunHarness (csvParser simpleValueParser))

        "SimpleArray",
        Samples.arrayExamples
        |> List.map (createRunHarness (arrayParser simpleValueParser))

        "LEqR", Samples.lEqRSamples |> List.map (createRunHarness (lEqR identifier identifier))

        "Identifier", Samples.identifierSamples |> List.map runIdentifierParser


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

        "values list",
        Samples.exampleQsItems
        |> runList (createRunHarness (ws >>. csvParser quotedString))

        "ValueList", Samples.exampleValueList |> runList (createRunHarness qsListParser)

        "ProjectEnvs", Samples.examplePEnv |> runList (createRunHarness projectEnvParser)

        "Project", Samples.exampleProj |> runList (createRunHarness projectParser)

        "ProjectList", Samples.exampleProjectList |> runList (createRunHarness projectListParser)

        "exampleAttachedProjectList", Samples.exampleAttachedProjectList |> runList (createRunHarness projectsParser)

        "JsonEncode",
        Samples.jsonEncodeExamples
        |> runList (createRunHarness JsonEncode.jsonencodeParser)
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
