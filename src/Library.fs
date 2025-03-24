//cspell: ignore jsonencode
module TfParse.Parsing

open FParsec

// Define the types to store parsed data

type SettingValue =
    | Bool of bool
    | Int of int
    | Float of float
    | Str of string
    | Other of obj

type ModuleSetting =
    | Providers of (string * string) list
    | Setting of string * SettingValue
    | Comment of string

type ModuleBlock = {
    Module: string
    Source: string
    Version: string option
    Settings: ModuleSetting list
}

// Parser for single-line comments (either # or //)
let singleLineComment: Parser<string, unit> =
    let sSpace = skipMany (pchar ' ' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r')

    //(pstring "#" <|> pstring "//") >>. manyTill anyChar (skipNewline) |>> (fun _ -> "Single-line comment") <?> "single-line comment"
    (pstring "#" <|> pstring "//") >>. sSpace >>. manyTill anyChar (skipNewline)
    |>> (fun x -> System.String(Array.ofList x))
    <?> "single-line comment"

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

// Define a parser for whitespace
let ws = skipMany (pchar ' ' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r')

// Define a parser for string literals (i.e., quoted strings)
let stringLiteral: Parser<string, unit> =
    between (pstring "\"") (pstring "\"") (manyChars (noneOf "\""))

open FParsec

// Parser for an identifier (starts with a letter or underscore, followed by letters, digits, or underscores)
let identifier: Parser<string, unit> =
    many1Satisfy (fun c -> System.Char.IsLetter(c) || c = '_') <?> "identifier"
    .>>. manySatisfy (fun c -> System.Char.IsLetterOrDigit(c) || c = '_')
    <?> "identifier continuation"
    |>> (fun (first, rest) -> first + rest)

// Parser for an integer number
let integer: Parser<int, unit> = pint32 <?> "integer"

// Parser for a floating-point number
let floatNumber: Parser<float, unit> = pfloat <?> "float"

// Parser for a boolean value (true or false)
let boolean: Parser<bool, unit> =
    (pstring "true" >>% true) <|> (pstring "false" >>% false) <?> "boolean"


let valueParser: Parser<SettingValue, unit> =
    choice [
        integer |>> Int
        floatNumber |>> Float
        boolean |>> Bool
        identifier |>> Str
        quotedString |>> Str
    ]


// Parser for a key-value pair inside the jsonencode block
let keyValuePair: Parser<string * SettingValue, unit> =
    stringLiteral .>> ws .>>. (pchar '=' >>. ws >>. valueParser)
    <?> "key-value pair"

// Parser for a JSON object (a block of key-value pairs enclosed in curly braces)
let jsonObject: Parser<_ list, unit> =
    between (pchar '{') (pchar '}') (sepBy keyValuePair (ws >>. pchar ',' .>> ws))
    <?> "JSON object"

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
    ws >>. lParser .>> ((ws .>> pstring "=" .>> ws) |>> ignore) .>>. rParser

let knownSetting text rParser =
    lEqR (pstring text) rParser |>> fun (_, r) -> r

// Parser for an unquoted setting, which could be an identifier, a number, or a boolean
let unquotedSetting: Parser<string * SettingValue, unit> =
    //identifier .>> ((ws .>> pstring "="  .>> ws) |>> ignore ) .>>. valueParser |>> fun x -> x
    lEqR identifier valueParser <?> "unquotedSetting"

let tObject: Parser<(string * SettingValue) list, unit> =
    between (pchar '{' >>. ws) (pchar '}' >>. ws) (attempt (many unquotedSetting))
    <?> "tObject"

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
    knownSetting "providers" tObject
    |>> function
        | [] -> List.empty
        | (k, Str v) :: [] -> [ k, v ]
        | multi -> failwith "multi provider not implemented"
    <?> "providers"

let mParser: Parser<ModuleSetting, unit> =
    choice [
        providersParser |>> fun x -> ModuleSetting.Providers(x)
        unquotedSetting |>> ModuleSetting.Setting
        commentLineParser |>> ModuleSetting.Comment
    ]
    <?> "mParser"
// Define the module parser that parses the whole module block
let moduleParser: Parser<ModuleBlock, unit> =
    pstring "module" >>. ws >>. stringLiteral .>> ws
    .>>. between
        (pchar '{' >>. ws)
        (pchar '}' >>. ws)
        (
        // Parse the source and version within the block
        (sourceParser .>> ws
         .>>. opt versionParser
         .>>. ws
         .>>. many (ws >>. mParser .>> ws)))
    |>> fun (m, (((source, versionOpt), _), s)) -> {
        Module = m
        Source = source
        Version = versionOpt
        Settings = s
    }

let multiModule: Parser<ModuleBlock list, unit> = many moduleParser

let createRunHarness title parser =
    fun (input: string) ->
        match run parser input with
        | Success(x, _, _) -> printfn "%s Parsed: %A" title x
        | Failure(errMsg, _, _) ->
            printfn "Parsing failed: %s" errMsg
            failwithf "%s Parser" title

let runVParser (input: string) =
    //match run versionParser input with
    //| Success(v, _, _) ->
    //    printfn "Parsed Version: %s" v
    //| Failure(errMsg, _, _) ->
    //    printfn "Parsing failed: %s" errMsg
    //    failwith "V Parser"
    createRunHarness "V" versionParser input

Samples.simpleVersion |> runVParser

let runSettingParser (input: string) =
    match run unquotedSetting input with
    | Success((k, v), _, _) -> printfn "Parsed Setting: %s = %A" k v
    | Failure(errMsg, _, _) ->
        printfn "Parsing failed: %s" errMsg
        failwith "S Parser"

[ "version = \"1.0.0\""; "enabled = true" ] |> List.iter runSettingParser

// Helper function to run the parser
let runParser (input: string) =
    match run moduleParser input with
    | Success(result, _, _) ->
        printfn "Parsed module: Source: %s, Version: %s" result.Source (result.Version |> Option.defaultValue "latest")
        printfn "Settings:"
        result.Settings |> List.iter (fun s -> printfn "\t%A" s)
    | Failure(errMsg, _, _) ->
        printfn "Parsing failed: %s" errMsg
        failwithf "Parser input: '%s'" input


let runParserMainSample () =
    // Example Terraform input
    runParser Samples.exampleInput // |> Dump |> ignore
