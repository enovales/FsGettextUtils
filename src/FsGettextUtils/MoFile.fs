module FsGettextUtils

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

/// <summary>
/// Handle for information about an MO file.
/// </summary>
type MoFileInfo = {
    originalStringLengthsAndOffsets: (uint32 * uint32) array
    translatedStringLengthsAndOffsets: (uint32 * uint32) array
}

/// <summary>
/// Represents a single original or translated string in the MO file.
/// Contains an optional context string, a singular form, and 
/// any number of plural forms of the string.
/// </summary>
type MoString = {
    context: string option
    singular: string
    pluralForms: string array
} with
    override this.ToString() = 
        let hexString(s: string) = 
            "\"" + (s.ToCharArray() |> Array.map(fun c -> (uint32 c).ToString("X2")) |> String.concat(", ")) + "\""
        let contextString = 
            match this.context with
            | Some(c) -> hexString(c)
            | _ -> "None"
        let singularString = hexString(this.singular)
        let pluralFormsString = this.pluralForms |> Array.map hexString |> String.concat(", ")
        "(" + contextString + ", " + singularString + ", " + "(" + pluralFormsString + "))"

/// <summary>
/// Discriminated union representing the state when reading an MO string.
/// </summary>
type ReadStringState = 
    | ContextOrSingular
    | Singular
    | PluralForms

/// <summary>
/// Accumulated state information while processing each character in the
/// MO string.
/// </summary>
type ReadStringTrackingState = 
    {
        state: ReadStringState
        acc: char array
        accIndex: int
        ms: MoString
    }

/// <summary>
/// Endian swapping function.
/// </summary>
let swapUint32(i: uint32) = 
    let c0 = (i >>> 0) &&& 0xfful
    let c1 = (i >>> 8) &&& 0xfful
    let c2 = (i >>> 16) &&& 0xfful
    let c3 = (i >>> 24) &&& 0xfful

    (c0 <<< 24) ||| (c1 <<< 16) ||| (c2 <<< 8) ||| c3


/// <summary>
/// Creates a wrapper for reading uint32s from the provided
/// reader, performing endian swapping if necessary.
/// </summary>
/// <param name="br">the binary reader to use</param>
/// <param name="needEndianSwap">whether endian swapping is needed</param>
let readUint32WithSwap(br: BinaryReader, needEndianSwap: bool)() = 
    if needEndianSwap then
        swapUint32(br.ReadUInt32())
    else
        br.ReadUInt32()

/// <summary>
/// Reads the header from the MO file, and creates a handle to use to
/// access the strings.
/// </summary>
let GetMoInfo(s: Stream) = 
    if (s.Seek(int64 0, SeekOrigin.Begin) <> int64 0) then
        failwith "couldn't seek to MO file header"

    use br = new BinaryReader(s, Encoding.Default, true)
    let magic = br.ReadUInt32()
    let swappedUint32Reader = readUint32WithSwap(br, (magic <> uint32 0x950412de))
    let version = swappedUint32Reader()

    // ***TODO: check version, abort if not a version we support
    let count = swappedUint32Reader()
    let originalStringsTableOffset = swappedUint32Reader()
    let translatedStringsTableOffset = swappedUint32Reader()

    if (s.Seek(int64 originalStringsTableOffset, SeekOrigin.Begin) <> int64 originalStringsTableOffset) then
        failwith "couldn't seek to original strings table"
    let originalStringsLengthsAndOffsets = Array.init(int count)(fun _ -> (swappedUint32Reader(), swappedUint32Reader()))

    if (s.Seek(int64 translatedStringsTableOffset, SeekOrigin.Begin) <> int64 translatedStringsTableOffset) then
        failwith "couldn't seek to translated strings table"
    let translatedStringsLengthsAndOffsets = Array.init(int count)(fun _ -> (swappedUint32Reader(), swappedUint32Reader()))

    {
        MoFileInfo.originalStringLengthsAndOffsets = originalStringsLengthsAndOffsets
        translatedStringLengthsAndOffsets = translatedStringsLengthsAndOffsets
    }

/// <summary>
/// Internal function for reading an MO string out of the stream.
/// </summary>
/// <param name="s">the stream</param>
/// <param name="e">the encoding to use</param>
/// <param name="length">the length of the string to extract, in bytes</param>
/// <param name="offset">the offset of the start of the string, in bytes</param>
let readString(s: Stream, e: Encoding)(length: uint32, offset: uint32) = 
    if (s.Seek(int64 offset, SeekOrigin.Begin) <> int64 offset) then
        failwith "couldn't seek to string origin"

    let bytes = Array.zeroCreate<byte>(int length)
    let bytesRead = s.Read(bytes, 0, int length)
    if bytesRead <> int length then
        failwith(String.Format("couldn't read enough bytes for string -- wanted {0} from origin {1}, got {2}", length, offset, bytesRead))

    let chars = e.GetChars(bytes)
        
    let foldFunc(s: ReadStringTrackingState)(c: char) = 
        match s.state with
        | ReadStringState.ContextOrSingular when c = char 4 ->
            // end of context
            { s with ms = { s.ms with context = Some(new String(s.acc, 0, s.accIndex)) }; state = ReadStringState.Singular; accIndex = 0 }
        | ReadStringState.ContextOrSingular when c = char 0 ->
            // end of singular
            { s with ms = { s.ms with singular = new String(s.acc, 0, s.accIndex) }; state = ReadStringState.PluralForms; accIndex = 0 }
        | ReadStringState.Singular when c = char 0 ->
            // end of singular, previously had a context
            { s with ms = { s.ms with singular = new String(s.acc, 0, s.accIndex) }; state = ReadStringState.PluralForms; accIndex = 0 }
        | ReadStringState.PluralForms when c = char 0 ->
            // end of one plural form
            let newPlural = new String(s.acc, 0, s.accIndex)
            { s with ms = { s.ms with pluralForms = [| newPlural |] |> Array.append(s.ms.pluralForms) }; accIndex = 0 }
        | _ ->
            Array.set(s.acc)(s.accIndex)(c)
            { s with accIndex = s.accIndex + 1 }

    let finalizeFunc(s: ReadStringTrackingState) = 
        match s.state with
        | ReadStringState.ContextOrSingular | ReadStringState.Singular -> 
            // either had no context, and just a singular, or a context with a singular. neither one with plural forms.
            { s with ms = { s.ms with singular = new String(s.acc, 0, s.accIndex) } }
        | ReadStringState.PluralForms ->
            // had plural forms.
            let newPlural = new String(s.acc, 0, s.accIndex)
            { s with ms = { s.ms with pluralForms = [| newPlural |] |> Array.append(s.ms.pluralForms) }; accIndex = 0 }

    let initialState = 
        { 
            ReadStringTrackingState.state = ReadStringState.ContextOrSingular
            ms = { MoString.context = None; singular = ""; pluralForms = [||] }
            acc = Array.zeroCreate<char>(int length)
            accIndex = 0
        }

    let finalState = chars |> Array.fold foldFunc initialState |> finalizeFunc
    finalState.ms
    
/// <summary>
/// Utility function for reading a seq of strings from an MO file, given an 
/// array of offsets and lengths.
/// </summary>
/// <param name="s">the stream to use</param>
/// <param name="e">the encoding to use for reading</param>
/// <param name="ob">the array of lengths and offsets to use</param>
/// <param name="indices">the indices in ob of the strings to extract</param>
let getStringsFromOffsetBlocks(s: Stream, e: Encoding, ob: (uint32 * uint32) array)(indices: int seq) = 
    let getStringByIndex(i: int) = readString(s, e)(ob.[i])
    indices |> Seq.map getStringByIndex

/// <summary>
/// Retrieves a seq of original strings from an MO file.
/// </summary>
/// <param name="s">the stream of the MO file</param>
/// <param name="info">the handle block returned from GetMoInfo</param>
/// <param name="encoding">the encoding to use for reading strings</param>
/// <param name="indices">the indices of the original strings to extract</param>
let GetOriginalStrings(s: Stream, info: MoFileInfo, encoding: Encoding)(indices: int seq) = 
    indices |> getStringsFromOffsetBlocks(s, encoding, info.originalStringLengthsAndOffsets)

/// <summary>
/// Retrieves a seq of translated strings from an MO file.
/// </summary>
/// <param name="s">the stream of the MO file</param>
/// <param name="info">the handle block returned from GetMoInfo</param>
/// <param name="encoding">the encoding to use for reading strings</param>
/// <param name="indices">the indices of the translated strings to extract</param>
let GetTranslatedStrings(s: Stream, info: MoFileInfo, encoding: Encoding)(indices: int seq) = 
    indices |> getStringsFromOffsetBlocks(s, encoding, info.translatedStringLengthsAndOffsets)

let private contentTypeCharSetRegex = new Regex(@"(Content-Type\:).*(charset.*=\s*)(?<charsetName>[0-9a-zA-Z\-_]+)")

/// <summary>
/// Optionally returns the string encoding name that matches the MIME charset type 
/// specified in the MIME headers passed in.
/// </summary>
/// <param name="h">the string containing the MIME headers to parse</param>
let getEncodingNameFromMIMEHeader(h: string) = 
    let extractCharsetName l = 
        contentTypeCharSetRegex.Match(l).Groups.Item("charsetName").Value

    h.Split([| "\n"; "\r" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.tryFind contentTypeCharSetRegex.IsMatch
    |> Option.map extractCharsetName

/// <summary>
/// Optionally returns the Encoding that matches the MIME charset type specified
/// in the MIME headers passed in.
/// </summary>
/// <param name="h">the string containing the MIME headers to parse</param>
let GetEncodingFromMIMEHeader(h: string) = 
    let matchesEncodingBodyName(n: string)(e: EncodingInfo) = 
        e.GetEncoding().BodyName = n

    getEncodingNameFromMIMEHeader(h)
    |> Option.bind (fun n -> Encoding.GetEncodings() |> Array.tryFind(matchesEncodingBodyName n))
    |> Option.map (fun e -> e.GetEncoding())
