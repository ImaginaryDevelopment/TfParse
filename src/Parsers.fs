module TfParse.Parsers

open FParsec

open BReuse

// Define the types to store parsed data

type SettingValue =
    | Bool of bool
    | Int of int
    | Float of float
    | Str of string
    | Array of SettingValue list
    | Other of (string * SettingValue) list

type ProjectEnvironment = {
    EId: string
    Vars: (string * SettingValue) list
}

type AttachedProject = {
    Project: string
    Envs: ProjectEnvironment list
}

type ModuleSetting =
    | Providers of (string * string) list
    | Setting of string * SettingValue
    | Comment of string
    | SettingBlock of string * (string * SettingValue) list
    | ValueList of string * string list
    | AttachedProjects of AttachedProject list


type ModuleBlock = {
    Module: string
    Source: string
    Version: string option
    Settings: ModuleSetting list
}

type ParserState = { ParserStack: string list }

type MyParser<'t> = Parser<'t, ParserState>

let pushState (parserName: string) : MyParser<unit> =
    updateUserState (fun state -> {
        state with
            ParserStack = parserName :: state.ParserStack
    })

let popState (parserName: string) : MyParser<unit> =
    updateUserState (fun state ->
        if state.ParserStack |> Seq.contains parserName |> not then
            failwithf "Parser pop without push: '%s'" parserName

        {
            state with
                ParserStack = state.ParserStack |> List.skipWhile (fun ps -> ps <> parserName) |> List.skip 1
        })

// let bridgeState (parser:Parser<_,unit>) x =
//     let us = getUserState x
//     let lifted = setUserState () x
//     let ss = x.CreateSubstream(x.)

//     parser lifted

let check = char "\u2713"
let xMark = char "\u2715"

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

// let haltOnFailure parserTitle parser (charStream: CharStream<_>) =
//     parser charStream
//     |> fun (x: Reply<_>) ->
//         match x.Status with
//         | Ok -> x
//         | FatalError
//         | Error ->
//             eprintfn "\t%s Halt: %A" parserTitle x.Status
//             eprintfn "\t%A" charStream.UserState
//             getErrors x.Error |> Option.iter (Seq.iter (eprintfn "\t%A"))

//             // failwithf "Parser %s failed" parserTitle
//             x
//         | _ ->
//             eprintfn "Bizarre status: %A" x.Status
//             x

// Define a parser for whitespace
let ws: MyParser<unit> =
    // skipMany (pchar ' ' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r') <??> "ws"
    spaces <?> "ws"

// Parser for single-line comments (either # or //)
let singleLineComment: MyParser<string> =
    let sSpace = skipMany (pchar ' ' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r')

    //(pstring "#" <|> pstring "//") >>. manyTill anyChar (skipNewline) |>> (fun _ -> "Single-line comment") <?> "single-line comment"
    pushState "singleLineComment"
    >>. sSpace
    >>. (pstring "#" <|> pstring "//")
    >>. sSpace
    // >>. manyTill anyChar newline
    >>. restOfLine true
    <?> "single-line comment"
    .>> popState "singleLineComment"

// |> haltOnFailure "singleLineComment"

// Parser for multi-line comments (between /* and */)
let multiLineComment: MyParser<string> =
    between (pstring "/*") (pstring "*/") (manyCharsTill anyChar (pstring "*/"))
    |>> (fun _ -> "Multi-line comment")
    <?> "multi-line comment"

// Parser that skips over whitespace and comments
let commentLineParser: MyParser<string> =
    //many (skipMany (singleLineComment <|> multiLineComment)) >>% "Comment line" <?> "comment line"
    singleLineComment <?> "comment line"


// Parser for a single character that is not a quote or backslash
let nonEscapeChar: Parser<char, _> =
    satisfy (fun c -> c <> '"' && c <> '\\') <?> "non-escaped character"

// Parser for escape sequences inside the string (e.g., \" or \\)
let escapeSequence: Parser<char, _> =
    pchar '\\' >>. (anyOf "\"\\") <?> "escape sequence"

// Parser for a quoted string (handling escapes and normal characters)
let quotedString: MyParser<string> =
    pushState "quotedString"
    >>. between (pchar '"') (pchar '"') (manyChars (nonEscapeChar <|> escapeSequence))
    .>> popState "quotedString"
    <?> "quoted string"



// Define a parser for string literals (i.e., quoted strings)
let stringLiteral: Parser<string, _> =
    between (pstring "\"") (pstring "\"") (manyChars (noneOf "\""))

// Parser for an identifier (starts with a letter or underscore, followed by letters, digits, or underscores)
let identifier: Parser<string, _> =
    many1Satisfy (fun c -> System.Char.IsLetter c || c = '_') <?> "identifier"
    .>>. manySatisfy (fun c -> System.Char.IsLetterOrDigit(c) || c = '_' || c = '.')
    <?> "identifier continuation"
    |>> (fun (first, rest) -> first + rest)
    <?> "identifier"

// Parser for an integer number
// does not appear to consume commas
// will consume just the 1 when given 1.5
let integer: MyParser<int> = pint32 <?> "integer"

let intDotIntParser =
    let mutable wholePart = 0
    let mutable decPart = 0

    integer
    .>>. (pchar '.' >>. notFollowedBy (pchar '-'))
    .>>. (integer
          |>> fun i ->
              decPart <- i
              $"0.%i{i}" |> float)
    .>> pushState $"{wholePart}.{decPart}"
    <?> "intDotInt"
    |>> fun ((w, ()), dec) ->
        (let r = if w >= 0 then float w + dec else float w - dec
         //  printfn "Made intDotInt %i.%f = %f" w dec r
         r)

// Parser for a floating-point number
let floatNumber: MyParser<float> = pfloat <?> "float"

// Parser for a boolean value (true or false)
let boolean: MyParser<bool> =
    (pstring "true" >>% true) <|> (pstring "false" >>% false) <?> "boolean"

let csvItemParser p =
    // let optComma = ws >>. opt (pchar ',') .>> ws |>> fun x -> ()
    // p .>> attempt (eof <|> optComma) <??> "csv-item"
    let optComma = attempt ws >>. optional (pchar ',') .>> attempt ws

    pushState "csv-item" >>. ws >>. p .>> popState "csv-item" <??> "csv-item"
    .>> attempt optComma

// any type of item comma delimited
// supports trailing commas, single item lists
let csvParser p =
    let trailOpt = opt (attempt (pchar ','))
    ws >>. many (attempt (p .>> (ws .>> trailOpt .>> ws)))
// let customMany1 = p

// let listParser =
//     ws >>. sepBy customMany1 (ws >>. pchar ',' .>> ws) <?> "sep-by-list"

// let items =
//     pushState "csvParser" >>. ws >>. listParser // many1 (csvItemParser p)
//     // pushState "csvParser" >>. many1 (csvItemParser p)
//     // >>. choice [ many1 (csvItemParser p); csvItemParser p |>> fun x -> [ x ] ]
//     //     attempt (sepEndBy p (ws >>. pchar ',' .>> ws) <??> "SepEndBy")
//     //     attempt (commaSepBy p)
//     //     // handle a single item, no comma list
//     //     (ws >>. p .>> ws |>> fun x -> [ x ]) <??> "SingleCsvItem"
//     .>> popState "csvParser"

// items <?> "csvParser"

let defaultEmptyList parseResult =
    parseResult |>> fun x -> x |> Option.defaultValue List.empty

let myBetween lParser rParser bodyParser =
    // between (lParser >>. ws) (ws .>> rParser) (bodyParser <?> "between-body")
    between (lParser >>. ws) (ws .>> rParser) bodyParser

// supports empty lists, trailing commas
let emptyArray = pchar '[' .>> ws .>> pchar ']' .>> ws <?> "empty array"

let arrayParser p =
    let trailOpt = opt (attempt (pchar ','))
    // let m1 = many1 (csvItemParser p)
    // myBetween (pchar '[') (pchar ']') (attempt (opt (csvParser p)) <?> "array-body" |> defaultEmptyList .>> ws)
    // myBetween (pchar '[') (pchar ']') (opt m1 <?> "array-body" |> defaultEmptyList .>> ws)
    // myBetween (pchar '[') (trailOpt >>. pchar ']') (sepBy (ws >>. p .>> ws) (ws .>> pchar ',' .>> ws) .>> trailOpt)
    choice [
        attempt emptyArray |>> fun _ -> List.empty
        between (pchar '[' >>. ws) (trailOpt >>. pchar ']') (many (attempt (p .>> (ws .>> trailOpt .>> ws))))
    ]
    <?> "array"
// (pchar '[') >>. (ws >>. (csvParser p) .>> ws) .>> (pchar ']') <?> "array"

// does not support commas in numbers
let settingValueParser: Parser<SettingValue, _> =
    let parserName = "simpleValue"

    pushState parserName
    >>. choice [
        attempt intDotIntParser |>> Float
        integer |>> Int
        floatNumber |>> Float
        boolean |>> Bool
        // unquoted identifier
        identifier |>> Str
        // quoted value
        quotedString |>> Str
    ]
    .>> popState parserName
    <??> "valueParser"


let lEqR lParser rParser =
    lParser .>> (ws .>> pstring "=" .>> ws |>> ignore) .>>. rParser

// none of this appears to properly handle a list
module JsonEncode =

    // right side of equals
    let nestedParser, nestedParserRef = createParserForwardedToRef<SettingValue, _> ()

    // Parser for a key-value pair inside the jsonencode block
    let keyValuePair: Parser<string * SettingValue, _> =
        pushState "keyValuePair"
        >>. ws
        >>. lEqR (stringLiteral <|> identifier) (ws >>. nestedParser .>> ws)
        .>> popState "keyValuePair"
        <??> "key-value pair"

    // might not need csv parser, example seen so far only have one prop
    // might not even be csv
    let jsonEncodeBody = csvParser keyValuePair <??> "Json Body"

    // Parser for a JSON object (a block of key-value pairs enclosed in curly braces)
    let jsonObject: Parser<_ list, _> =
        between (pchar '{') (pchar '}') jsonEncodeBody <??> "JSON object"

    // array of values, array of objects, empty array, object
    nestedParserRef.Value <-
        ws
        >>. choice [
            // support an array of simple values, or objects, not arrays
            attempt (
                arrayParser (
                    choice [
                        settingValueParser <??> "NestedArray"
                        jsonObject |>> fun x -> x |> SettingValue.Other
                    ]
                )
                |>> fun x -> SettingValue.Array x
            )
            emptyArray >>% SettingValue.Array List.empty
            attempt settingValueParser <??> "setting value"
            jsonObject <??> "JsonObject" |>> SettingValue.Other
        ]
        .>> ws
        <??> "Nested"

    // Parser for the jsonencode function call
    let jsonencodeParser: Parser<_, _> =
        pstring "jsonencode" >>. ws >>. between (pchar '(') (pchar ')') jsonObject
        <??> "jsonencode block"

let knownSetting text rParser =
    lEqR (pstring text) rParser |>> fun (_, r) -> r

// Parser for an unquoted setting, which could be an identifier, a number, or a boolean
let unquotedSetting: MyParser<string * SettingValue> =
    //identifier .>> ((ws .>> pstring "="  .>> ws) |>> ignore ) .>>. valueParser |>> fun x -> x
    ws >>. lEqR identifier settingValueParser <??> "unquotedSetting"

let anySimpleSetting = ws >>. lEqR (identifier <|> stringLiteral) settingValueParser

let tObjectBody =
    ws >>. many (attempt (ws >>. JsonEncode.keyValuePair)) .>> ws <??> "tObjectBody"

// let mustSucceed (p:Parser<_,_>) (input: CharStream<'b>) =
//     match p input with
//     | Success (a,b,c) -> Success (a,b,c)

let tObject: MyParser<(string * SettingValue) list> =

    // let tBody =
    // between (pchar '{' >>. ws) (pchar '}' >>. ws) (attempt (many unquotedSetting))
    ws >>. between (pchar '{' >>. ws) (ws >>. pchar '}') (ws >>. tObjectBody)
    <??> "tObject"

// Define a parser for the "source" keyword and its value
let sourceParser: MyParser<string> =
    //pstring "source" >>. ws >>. pstring "=" >>. ws >>. stringLiteral
    knownSetting "source" stringLiteral <?> "source"

// Define a parser for the "version" keyword and its value (optional)
let versionParser: MyParser<string> =
    pushState "version"
    >>. pstring "version"
    >>. ws
    >>. pstring "="
    >>. ws
    >>. choice [ stringLiteral; quotedString ]
    .>> popState "version"
    <?> "version"

// assume one provider for now?
let providersParser: MyParser<(string * string) list> =
    knownSetting "providers" tObject <?> "providers"
    |>> function
        | [] -> List.empty
        | (k, Str v) :: [] -> [ k, v ]
        | multi -> failwith "multi provider not implemented"
    <?> "providers"

let descriptionBlockParser: MyParser<string> =
    // let nonEotItem = manyChars (noneOf ['\r';'\n']) .>> newline
    let nonEotItem = charsTillString "EOT" false System.Int32.MaxValue
    let eotBlock = between (pstring "EOT" .>> ws) (pstring "EOT" >>. ws) nonEotItem

    pushState "description"
    >>. ws
    >>. pstring "description"
    >>. ws
    >>. pchar '='
    >>. ws
    >>. pstring "<<-"
    >>. ws
    >>. eotBlock
    .>> popState "description"

let tQObject =
    let tqObjectBody =
        ws >>. many (ws >>. anySimpleSetting .>> ws) .>> ws <??> "tqObjectBody"

    between (pchar '{' >>. ws) (ws >>. pchar '}') (ws >>. tqObjectBody)
    <?> "tqObject"

let settingBlockParser: MyParser<string * (string * SettingValue) list> =
    lEqR (ws >>. identifier) tQObject <??> "setting block"

let myBetweenChars l r bodyParser =
    myBetween (pchar l) (pchar r) (ws >>. attempt bodyParser .>> ws)

let qsListParser =
    let items = quotedString
    let list = arrayParser items
    list

let valueListBlockParser = lEqR (ws >>. identifier) qsListParser

let idParser = lEqR (pstring "id") identifier |>> snd
let varsParser = lEqR (pstring "vars") tObject |>> snd

// { id = ... \r\n vars = {}}
let projectEnvParser =
    let body = ws >>. idParser .>>. (ws >>. varsParser)
    myBetweenChars '{' '}' body <?> "ProjectEnv"

// [ items ]
let envsListParser =
    let item = projectEnvParser |>> fun (envId, vars) -> { EId = envId; Vars = vars }
    arrayParser item <?> "EnvsList"

// envs = [ ..., ... ]
let envsBlockParser = lEqR (pstring "envs") envsListParser |>> snd <??> "EnvsBlock"

// { project = ... \r\n envs = [ ... ]}
let projectParser =
    let projectLine = lEqR (pstring "project") quotedString |>> snd <??> "Project-Line"

    let bodyParser =
        ws >>. projectLine .>>. (ws >>. envsBlockParser .>> ws) <??> "Project-Body"

    myBetweenChars '{' '}' bodyParser
    |>> fun (pl, envs) -> { Project = pl; Envs = envs }
    <?> "Project"

let projectListParser =
    // myBetweenChars '[' ']' (csvParser projectParser) <??> "ProjectList"
    arrayParser projectParser <?> "ProjectList-Array"

let attachedProjectsParser =
    lEqR (pstring "attached_projects") projectListParser |>> snd <?> "Projects"

let mParser: MyParser<ModuleSetting> =
    ws
    >>. choice [
        attempt providersParser |>> fun x -> ModuleSetting.Providers(x)

        attempt descriptionBlockParser
        |>> fun x -> ModuleSetting.Setting("description", Str x)


        attempt settingBlockParser |>> fun x -> ModuleSetting.SettingBlock x

        attachedProjectsParser |>> ModuleSetting.AttachedProjects

        attempt unquotedSetting |>> ModuleSetting.Setting

        valueListBlockParser |>> ModuleSetting.ValueList

        (ws >>. commentLineParser .>> ws) |>> ModuleSetting.Comment

    ]
    .>> ws
    <?> "mParser"

// Define the module parser that parses the whole module block
let moduleParser: MyParser<ModuleBlock> =
    let parseModuleBody =
        // Parse the source and version within the block
        sourceParser .>> ws
        .>>. opt versionParser
        .>>. ws
        .>>. many (ws >>. mParser .>> ws)

    pstring "module" >>. ws >>. stringLiteral .>> ws
    .>>. between (pchar '{' >>. ws) (pchar '}' >>. ws) parseModuleBody

    |>> fun (m, (((source, versionOpt), _), s)) -> {
        Module = m
        Source = source
        Version = versionOpt
        Settings = s
    }

let multiModule: MyParser<ModuleBlock list> = many moduleParser



let replyToParserResult (stream: CharStream<_>) (reply: Reply<_>) =

    if reply.Status = Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        Failure(error.ToString(stream), error, stream.UserState)
