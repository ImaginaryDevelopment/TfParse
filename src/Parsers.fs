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
let ws = skipMany (pchar ' ' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r')

// Parser for single-line comments (either # or //)
let singleLineComment: Parser<string, unit> =
    let sSpace = skipMany (pchar ' ' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r')

    //(pstring "#" <|> pstring "//") >>. manyTill anyChar (skipNewline) |>> (fun _ -> "Single-line comment") <?> "single-line comment"
    sSpace
    >>. (pstring "#" <|> pstring "//")
    >>. sSpace
    // >>. manyTill anyChar newline
    >>. restOfLine true
    <?> "single-line comment"

// |> haltOnFailure "singleLineComment"

// Parser for multi-line comments (between /* and */)
let multiLineComment: Parser<string, unit> =
    between (pstring "/*") (pstring "*/") (manyCharsTill anyChar (pstring "*/"))
    |>> (fun _ -> "Multi-line comment")
    <?> "multi-line comment"

// Parser that skips over whitespace and comments
let commentLineParser: Parser<string, unit> =
    //many (skipMany (singleLineComment <|> multiLineComment)) >>% "Comment line" <?> "comment line"
    singleLineComment <?> "comment line"


// Parser for a single character that is not a quote or backslash
let nonEscapeChar: Parser<char, unit> =
    satisfy (fun c -> c <> '"' && c <> '\\') <?> "non-escaped character"

// Parser for escape sequences inside the string (e.g., \" or \\)
let escapeSequence: Parser<char, unit> =
    pchar '\\' >>. (anyOf "\"\\") <?> "escape sequence"

// Parser for a quoted string (handling escapes and normal characters)
let quotedString: Parser<string, unit> =
    between (pchar '"') (pchar '"') (manyChars (nonEscapeChar <|> escapeSequence))
    <?> "quoted string"


// Define a parser for string literals (i.e., quoted strings)
let stringLiteral: Parser<string, unit> =
    between (pstring "\"") (pstring "\"") (manyChars (noneOf "\""))

let createRunHarness title parser =
    fun (input: string) ->
        match run parser input with
        | Success(x, _, _) -> sprintf "%s Parsed: %A ('%s')" title x (String.truncate 10 input) |> Result.Ok
        | Failure(errMsg, _, _) -> Result.Error(sprintf "Parsing failed: %s" errMsg, sprintf "%s Parser" title)

// Parser for an identifier (starts with a letter or underscore, followed by letters, digits, or underscores)
let identifier: Parser<string, unit> =
    many1Satisfy (fun c -> System.Char.IsLetter(c) || c = '_') <?> "identifier"
    .>>. manySatisfy (fun c -> System.Char.IsLetterOrDigit(c) || c = '_' || c = '.')
    <?> "identifier continuation"
    |>> (fun (first, rest) -> first + rest)
    <?> "identifier"

// Parser for an integer number
let integer: Parser<int, unit> = pint32 <?> "integer"

// Parser for a floating-point number
let floatNumber: Parser<float, unit> = pfloat <?> "float"

// Parser for a boolean value (true or false)
let boolean: Parser<bool, unit> =
    (pstring "true" >>% true) <|> (pstring "false" >>% false) <?> "boolean"

// any type of item comma delimited
// supports trailing commas, single item lists
let csvParser p =
    let items =
        ws
        >>. choice [
            attempt (sepEndBy p (ws >>. pchar ',' .>> ws))
            attempt (sepBy p (ws >>. pchar ',' .>> ws))
            // handle a single item, no comma list
            (ws >>. p .>> ws) |>> fun x -> [ x ]
        ]
        <?> "csvParser"
        .>> ws

    items

let myBetween lParser rParser bodyParser =
    between (lParser >>. ws) (rParser >>. ws) (bodyParser <?> "array body")

// supports empty lists, trailing commas
let arrayParser p =
    myBetween (pchar '[') (pchar ']') (ws >>. (csvParser p) .>> ws)

let simpleValueParser: Parser<SettingValue, unit> =
    choice [
        integer |>> Int
        floatNumber |>> Float
        boolean |>> Bool
        // unquoted identifier
        identifier |>> Str
        // quoted value
        quotedString |>> Str
    ]
    <??> "valueParser"



// none of this appears to properly handle a list
module JsonEncode =

    let nestedParser, nestedParserRef =
        createParserForwardedToRef<SettingValue, unit> ()

    // Parser for a key-value pair inside the jsonencode block
    let keyValuePair: Parser<string * SettingValue, unit> =
        (stringLiteral <|> identifier) .>> ws .>>. (pchar '=' >>. ws >>. nestedParser)
        <?> "key-value pair"

    // Parser for a JSON object (a block of key-value pairs enclosed in curly braces)
    let jsonObject: Parser<_ list, unit> =
        between (pchar '{') (pchar '}') (csvParser keyValuePair) <?> "JSON object"

    nestedParserRef.Value <-
        choice [
            simpleValueParser
            jsonObject |>> SettingValue.Other
            // support an array of simple values, or objects, not arrays
            arrayParser (jsonObject |>> SettingValue.Other <|> simpleValueParser)
            |>> fun x -> SettingValue.Array x
        ]
    // Parser for the jsonencode function call
    let jsonencodeParser: Parser<_, unit> =
        pstring "jsonencode" >>. ws >>. between (pchar '(') (pchar ')') jsonObject
        <?> "jsonencode block"
//
//let jsonEncoded : Parser<obj, unit> =
//    pstring "jsonencode" >>. ws >> between (pstring "({" >>. ws) (pstring "})" >>. ws) (
//        many anyChar
//    )
//    |>> fun x -> x

let lEqR lParser rParser =
    lParser .>> (ws .>> pstring "=" .>> ws |>> ignore) .>>. rParser


let knownSetting text rParser =
    lEqR (pstring text) rParser |>> fun (_, r) -> r

// Parser for an unquoted setting, which could be an identifier, a number, or a boolean
let unquotedSetting: Parser<string * SettingValue, unit> =
    //identifier .>> ((ws .>> pstring "="  .>> ws) |>> ignore ) .>>. valueParser |>> fun x -> x
    ws >>. lEqR identifier simpleValueParser <??> "unquotedSetting"


let anySimpleSetting = ws >>. lEqR (identifier <|> stringLiteral) simpleValueParser

let runUnquotedSParser (input: string) =
    createRunHarness "US" unquotedSetting input

let tObjectBody =
    ws >>. many (ws >>. anySimpleSetting .>> ws) .>> ws <??> "tObjectBody"

let runTObjectBodyParser input =
    createRunHarness "tObjectBody" tObjectBody input

let tObject: Parser<(string * SettingValue) list, unit> =

    // let tBody =
    // between (pchar '{' >>. ws) (pchar '}' >>. ws) (attempt (many unquotedSetting))
    between (pchar '{' >>. ws) (ws >>. pchar '}') (ws >>. tObjectBody) <?> "tObject"

let runTObjectParser input =
    createRunHarness "tObject" tObject input

// Define a parser for the "source" keyword and its value
let sourceParser: Parser<string, unit> =
    //pstring "source" >>. ws >>. pstring "=" >>. ws >>. stringLiteral
    knownSetting "source" stringLiteral <?> "source"

// Define a parser for the "version" keyword and its value (optional)
let versionParser: Parser<string, unit> =
    pstring "version"
    >>. ws
    >>. pstring "="
    >>. ws
    >>. choice [ stringLiteral; quotedString ]
    <?> "version"

// assume one provider for now?
let providersParser: Parser<(string * string) list, unit> =
    knownSetting "providers" tObject <?> "providers"
    |>> function
        | [] -> List.empty
        | (k, Str v) :: [] -> [ k, v ]
        | multi -> failwith "multi provider not implemented"
    <?> "providers"

let descriptionBlockParser: Parser<string, unit> =
    // let nonEotItem = manyChars (noneOf ['\r';'\n']) .>> newline
    let nonEotItem = charsTillString "EOT" false System.Int32.MaxValue
    let eotBlock = between (pstring "EOT" .>> ws) (pstring "EOT" >>. ws) nonEotItem

    ws
    >>. pstring "description"
    >>. ws
    >>. pchar '='
    >>. ws
    >>. pstring "<<-"
    >>. ws
    >>. eotBlock


let tQObject =
    let tqObjectBody =
        ws >>. many (ws >>. anySimpleSetting .>> ws) .>> ws <??> "tqObjectBody"

    between (pchar '{' >>. ws) (ws >>. pchar '}') (ws >>. tqObjectBody)
    <?> "tqObject"

let settingBlockParser: Parser<string * (string * SettingValue) list, unit> =
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
    let items =
        csvParser (projectEnvParser |>> fun (envId, vars) -> { EId = envId; Vars = vars })

    myBetween (pchar '[') (pchar ']') (ws >>. attempt items .>> ws) <?> "EnvsList"


// envs = [ ..., ... ]
let envsBlockParser = lEqR (pstring "envs") envsListParser |>> snd <?> "EnvsBlock"

// { project = ... \r\n envs = [ ... ]}
let projectParser =
    let projectLine = lEqR (pstring "project") quotedString |>> snd
    let bodyParser = ws >>. projectLine .>>. (ws >>. envsBlockParser .>> ws)

    myBetweenChars '{' '}' bodyParser
    |>> fun (pl, envs) -> { Project = pl; Envs = envs }
    <?> "Project"

let projectListParser =
    myBetweenChars '[' ']' (csvParser projectParser) <??> "ProjectList"

let projectsParser =
    lEqR (pstring "attached_projects") projectListParser |>> snd <??> "Projects"

let mParser: Parser<ModuleSetting, unit> =
    ws
    >>. choice [
        attempt providersParser |>> fun x -> ModuleSetting.Providers(x)

        attempt descriptionBlockParser
        |>> fun x -> ModuleSetting.Setting("description", Str x)


        attempt settingBlockParser |>> fun x -> ModuleSetting.SettingBlock x

        projectsParser |>> ModuleSetting.AttachedProjects

        attempt unquotedSetting |>> ModuleSetting.Setting

        valueListBlockParser |>> ModuleSetting.ValueList

        (ws >>. commentLineParser .>> ws) |>> ModuleSetting.Comment

    ]
    .>> ws
    <?> "mParser"

// Define the module parser that parses the whole module block
let moduleParser: Parser<ModuleBlock, unit> =
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

let multiModule: Parser<ModuleBlock list, unit> = many moduleParser


let runVersionParser (input: string) =
    createRunHarness "V" versionParser input

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
