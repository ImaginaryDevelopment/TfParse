Samples.csvExamples
|> List.mapi( fun i (item,expected) ->
    let result = Runners.runAParser (Parsers.csvParser Parsers.simpleValueParser) item
    //let result = Runners.runAParser (Parsers.simpleValueParser) v
    match result with
    | Ok(value,pos) ->
        // assuming we're only dealing with single lines for now
        let remainder = item[int pos.Index..]
        
        let posDisplay = sprintf "%i,Ln: %i, Col: %i, Rem: %i" pos.Index pos.Line pos.Column remainder.Length
        i, string value,posDisplay,item, remainder
    | Error(msg,e,()) ->
        i, msg, string e, item, item
)
|> fun x -> x.Dump()
TfParse.Runners.runParserMainSample true