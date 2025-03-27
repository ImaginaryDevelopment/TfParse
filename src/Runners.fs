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

let formatSettingValue (k, v: SettingValue) = sprintf "Parsed Setting: %s = %A" k v
// let runSettingParser indent (input: string) =
//     match runOnString "" unquotedSetting input with
//     | Success((k, v), _, _) ->  |> Result.Ok
//     | Failure(errMsg, _, _) -> Result.Error(sprintf "%sParsing failed: %s" indent errMsg, "S Parser")

// let runIdentifierParser input = createRunHarness identifier input

// let runUnquotedSParser (input: string) = createRunHarness unquotedSetting input

// let runTObjectBodyParser input = createRunHarness tObjectBody input

// let runTObjectParser input = createRunHarness tObject input

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

open TfParse.Samples

let runValidationHarness (parser: MyParser<'t>) fOpt index (fts: Samples.FullTestSample<'t>) : TestResult =
    let tryRun () =
        try
            runOnString "" parser fts.Input |> Result.Ok
        with ex ->
            {
                Message = ex.Message
                Detail = "exn"
                Position = null
                State = List.empty
            }
            |> Result.Error

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

    let makeResult r = {
        Index = index
        Input = fts.Input
        Result = r
    }

    tryRun ()
    |> function
        | Result.Error x -> x |> Result.Error |> makeResult
        | Result.Ok r ->

            match r, fts.Expected with
            | Success(x: 't, us, pos), Result.Error() ->
                makeResult (
                    Result.Error {
                        Message = $"Expected failure, finished at %A{pos}"
                        Detail = string x
                        Position = pos
                        State = us.ParserStack
                    }
                )
            | Failure(errMsg, pe, us), Result.Ok v ->
                {
                    Message = $"Expected success at %i{pe.Position.Index}: %s{errMsg}"
                    Detail = string pe.Messages
                    State = us.ParserStack
                    Position = pe.Position
                }
                |> Result.Error
                |> makeResult

            | Failure(_, _, _), Result.Error() -> Result.Ok "Parser Failed Successfully" |> makeResult

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
                    | Result.Ok parsedValue ->
                        match fOpt with
                        | None -> sprintf "Parsed: %A ('%s')" x (String.truncate 10 fts.Input)
                        | Some f -> f parsedValue
                        |> Result.Ok
                        |> makeResult
                    | Result.Error(m1, m2) ->
                        {
                            Message = m1
                            Detail = m2
                            Position = pos
                            State = us.ParserStack
                        }
                        |> Result.Error
                        |> makeResult

let adaptOldTest (p: MyParser<'t>) fOpt index item =
    runValidationHarness p fOpt index {
        Input = item
        Expected = Result.Ok None
        Validators = List.empty
    }

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

    let indent = "\t"


    // for samples where the length should be the total length

    [
        // "Space", Samples.wsSamples |> List.map (createRunHarness ws)
        "Space", Samples.wsSamples |> List.mapi (runValidationHarness ws None)
        "Int", Samples.intSamples |> List.mapi (runValidationHarness integer None)
        "IntDotInt", Samples.idiSamples |> List.mapi (runValidationHarness intDotIntParser None)
        "Value", Samples.valuesSimple |> List.mapi (adaptOldTest settingValueParser None)

        "Comment", Samples.commentSamples |> List.mapi (adaptOldTest singleLineComment None)

        "Version", Samples.simpleVersion |> List.mapi (runValidationHarness versionParser None)

        "CsvItem",
        Samples.csvItems
        |> List.mapi (runValidationHarness (csvItemParser settingValueParser) None)
        "CsvTuple",
        Samples.csvTuple
        |> List.mapi (runValidationHarness (csvItemParser integer .>>. csvItemParser integer) None)

        "SimpleArray",
        Samples.arrayExamples
        |> List.mapi (adaptOldTest (arrayParser settingValueParser) None)

        "Csv",
        Samples.csvExamples
        |> List.mapi (runValidationHarness (csvParser settingValueParser) None)

        "LEqR",
        Samples.lEqRSamples
        |> List.mapi (runValidationHarness (lEqR identifier identifier) None)

        "Identifier", Samples.identifierSamples |> List.mapi (runValidationHarness identifier None)


        "UnquotedSetting", Samples.unquotedSettingSamples |> List.mapi (adaptOldTest unquotedSetting None)

        "Setting",
        [
            "version = \"1.0.0\""
            "enabled = true"
            "octopus = octopus "
            "octopus=octopusdeploy"
        ]
        |> List.mapi (adaptOldTest settingValueParser None)

        "tObjectBody", Samples.tObjectBodySamples |> List.mapi (adaptOldTest tObjectBody None)
        "tObject", Samples.tObjectSamples |> List.mapi (adaptOldTest tObject None)

        "description",
        Samples.exampleDescription
        |> List.mapi (adaptOldTest descriptionBlockParser None)

        "SettingBlock", Samples.exampleSettingBlocks |> List.mapi (adaptOldTest settingBlockParser None)

        "csv quoted list",
        Samples.quotedStringItems
        |> List.mapi (adaptOldTest (ws >>. csvParser quotedString) None)

        "QuotedString List", Samples.quotedStringList |> List.mapi (adaptOldTest qsListParser None)

        "JsonEncodeKvp", Samples.jsonEncodeKvp |> List.mapi (adaptOldTest JsonEncode.keyValuePair None)
        "JsonEncodeBody",
        Samples.jsonObjects
        |> List.mapi (runValidationHarness JsonEncode.jsonEncodeBody None)
        "JsonEncode",
        Samples.jsonEncodeExamples
        |> List.mapi (adaptOldTest JsonEncode.jsonencodeParser None)

        "ProjectEnvs", Samples.projectEnv |> List.mapi (adaptOldTest projectEnvParser None)

        "Project", Samples.exampleProj |> List.mapi (adaptOldTest projectParser None)

        "ProjectList", Samples.exampleProjectList |> List.mapi (adaptOldTest projectListParser None)

        "exampleAttachedProjectList",
        Samples.exampleAttachedProjectList
        |> List.mapi (adaptOldTest attachedProjectsParser None)

        "Ms", Samples.exampleMs |> List.mapi (adaptOldTest mParser None)
    ]
    |> List.iter (fun (title, results) ->
        let successMsg = sprintf "%s%c %s" indent check title

        let hasError =
            results
            |> Seq.exists (function
                | { Result = Result.Error _ } -> true
                | _ -> false)

        if hasError then
            eprintfn "%s parser failed:" title

            results
            |> List.sortByDescending (fun r -> Result.isOk r.Result, r.Index * -1)
            |> List.choose (fun r ->
                match r.Result with
                | Result.Ok msg ->
                    printfn "%s %i %s" successMsg r.Index msg
                    None
                | Result.Error tf ->
                    eprintfn "%s%c %i" indent xMark r.Index
                    Some(r.Index, tf.Message, tf.Detail)

            )
            |> fun x ->
                printfn "******************"
                x
            |> List.iter (fun (index, e, f) ->
                if String.isValueString e then
                    if e.Contains("\r") || e.Contains("\n") then
                        printfn "%sInd:%i:" indent index
                        indentLines (indent + indent) e |> eprintfn "%s"
                    else
                        printfn "%i:%s" index e
                // consider indenting this value if it is multi-line
                if String.isValueString f then
                    if f.Contains("\r") || f.Contains("\n") then
                        eprintfn "%sInd:%i:" indent index
                        indentLines (indent + indent) f |> eprintfn "%s"
                    else
                        eprintfn "%sInd:%i:%s" indent index f

                printfn "%s^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" indent
                printfn "")


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
