module BReuse

let (|ValueString|NullString|EmptyString|WhiteString|) x =
    if isNull x then NullString
    elif System.String.IsNullOrEmpty x then EmptyString
    elif System.String.IsNullOrWhiteSpace x then WhiteString
    else ValueString x

module String =
    open System

    let isValueString =
        function
        | ValueString _ -> true
        | _ -> false

    let truncate limit text =
        let minLimit = 7

        if limit < minLimit then
            invalidArg "limit" $"must be %i{minLimit} or more"

        match text with
        | NullString -> "<null>"
        | EmptyString -> "<empty>"
        | _ ->
            if text.Length < minLimit then
                text
            elif text.Length > limit then
                text.Substring(0, limit - 3) + "..." //[0.. text.Length - 3  ] + "..."
            else
                text

    let indexOf delimiter (text: string) =

        if String.IsNullOrEmpty delimiter then
            failwith "Bad delimiter"

        if String.IsNullOrEmpty text then
            None
        else
            let i = text.IndexOf delimiter
            if i >= 0 then Some i else None

let trim text =
    if System.String.IsNullOrEmpty text then
        text
    else
        text.Trim()

let trimEnd text =
    if System.String.IsNullOrEmpty text then
        text
    else
        text.TrimEnd()
