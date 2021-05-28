module TestUtils

open FsCheck
open FsGettextUtils
open System.IO
open System.Text

/// <summary>
/// Recursively returns power-of-two based smaller sizes.
/// </summary>
/// <param name="n">remaining input size</param>
let rec private powerOfTwoShrinksInternal(n: int) = 
    match n with
    | n when n > 0 -> n :: powerOfTwoShrinksInternal(n / 2)
    | _ -> [0]

/// <summary>
/// Returns the set of power-of-two based smaller sizes that
/// should be used to shrink input.
/// </summary>
/// <param name="n">input size for which shrinks should be produced</param>
let rec internal powerOfTwoShrinks(n: int) = 
    match n with
    | n when n > 1 -> powerOfTwoShrinksInternal(n / 2)
    | 2 -> [1; 0]
    | 1 -> [0]
    | _ -> []

/// <summary>
/// Shrinker that uses the power-of-two sizing functions
/// to produce shrinks.
/// </summary>
/// <param name="vals">array to shrink</param>
let internal shrinkArrayLog<'a>(vals: 'a array) = 
    let shrinks = powerOfTwoShrinks(vals.Length) |> Seq.ofList
    shrinks |> Seq.map (fun n -> vals |> Array.take(n))

/// <summary>
/// Record type containing paired string arrays.
/// </summary>
type OriginalAndTranslatedStrings = 
    {
        originalStrings: MoString array
        translatedStrings: MoString array
    }

/// <summary>
/// Container of information needed to serialize a MO file.
/// </summary>
type StringsAndFileInfo = 
    {
        magic: uint32
        formatVersion: uint32
        headerPadding: byte array
        strings: OriginalAndTranslatedStrings
        hashTable: uint32 array
        footerPadding: byte array
    }

/// <summary>
/// Magic number values that should be considerd as valid. Covers
/// both endianness possibilities.
/// </summary>
let internal validMagicNumbers = [| 0x950412deu; 0xde120495u |]
let internal validFileFormatVersions = [| 0u |]

let private writeUint32WithSwap(bw: BinaryWriter, needEndianSwap: bool)(i: uint32) = 
    if needEndianSwap then
        bw.Write(swapUint32(i))
    else
        bw.Write(i)

let internal getStringBytes(encoding: Encoding)(ms: MoString) = 
    let nullBytes = encoding.GetBytes([| char 0 |])
    let contextBytes = 
        match ms.context with
        | Some(context) -> 
            let contextDelimiter = encoding.GetBytes([| char 4 |])
            Array.concat([| encoding.GetBytes(context); contextDelimiter |])
        | _ -> [||]

    let pluralBytes = 
        match ms.pluralForms with
        | [||] -> [||]
        | pfs -> 
            let joined = pfs |> Array.collect(fun pf -> Array.append(encoding.GetBytes(pf))(nullBytes))
            joined |> Array.take(joined.Length - nullBytes.Length) |> Array.append(nullBytes)

    Array.concat([| contextBytes; encoding.GetBytes(ms.singular); pluralBytes |])

let private writeHeaderForSAFI(safi: StringsAndFileInfo, bw: BinaryWriter) = 
    let wrappedWriter = writeUint32WithSwap(bw, (safi.magic <> uint32 0x950412de))
    wrappedWriter(safi.formatVersion)
    wrappedWriter(uint32 safi.strings.originalStrings.Length)

    // keep track of the header offsets for things we need to fill in at the end.
    let originalStringOffsetTablePos = bw.BaseStream.Position
    wrappedWriter(uint32 0)
    let translatedStringOffsetTablePos = bw.BaseStream.Position
    wrappedWriter(uint32 0)

    wrappedWriter(uint32 safi.hashTable.Length)
    let hashTableOffsetPos = bw.BaseStream.Position
    wrappedWriter(uint32 0)

    bw.Write(safi.headerPadding)

    (uint32 originalStringOffsetTablePos, uint32 translatedStringOffsetTablePos, uint32 hashTableOffsetPos)

let internal writeByteStreamForSAFI(s: Stream, safi: StringsAndFileInfo, encoding: Encoding) = 
    let bw = new BinaryWriter(s)
    let nullBytes = encoding.GetBytes([| char 0 |])

    bw.Write(safi.magic)
    let wrappedWriter = writeUint32WithSwap(bw, (safi.magic <> uint32 0x950412de))

    // Retain the positions where we need to write the offsets of the 
    // original string, translated string, and hash tables.
    let (osOffsetTablePos, tsOffsetTablePos, htOffsetPos) = writeHeaderForSAFI(safi, bw)

    // Now we need to write dummy values for the original string and translated string
    // tables.
    let osOffsetTableStart = uint32 s.Position
    safi.strings.originalStrings |> Array.iter(fun _ -> wrappedWriter(uint32 0); wrappedWriter(uint32 0))
    let tsOffsetTableStart = uint32 s.Position
    safi.strings.translatedStrings |> Array.iter(fun _ -> wrappedWriter(uint32 0); wrappedWriter(uint32 0))

    // Write the hash table
    let htStart = uint32 s.Position
    safi.hashTable |> Array.iter(bw.Write)

    // Write each original string, with terminating null, and track the offset and size of each.
    // Note that the size specifically does not include the terminating null.
    let writeStringByteArray(ba: byte array) = 
        let offset = uint32 s.Position
        let size = uint32 ba.Length
        bw.Write(ba)
        bw.Write(nullBytes)
        (offset, size)

    let originalStringBytes = safi.strings.originalStrings |> Array.map(getStringBytes(encoding))
    let osOffsets = originalStringBytes |> Array.map writeStringByteArray

    // write each translated string, with terminating null, and track the offset of each
    let translatedStringBytes = safi.strings.translatedStrings |> Array.map(fun s -> getStringBytes(encoding)({ s with context = None }))
    let tsOffsets = translatedStringBytes |> Array.map writeStringByteArray

    bw.Write(safi.footerPadding)

    // go back and fill in all of the header offsets that need to be written
    s.Seek(int64 osOffsetTablePos, SeekOrigin.Begin) |> ignore
    wrappedWriter(osOffsetTableStart)
    s.Seek(int64 tsOffsetTablePos, SeekOrigin.Begin) |> ignore
    wrappedWriter(tsOffsetTableStart)
    s.Seek(int64 htOffsetPos, SeekOrigin.Begin) |> ignore
    wrappedWriter(htStart)

    // fill in the string offsets
    s.Seek(int64 osOffsetTableStart, SeekOrigin.Begin) |> ignore
    osOffsets |> Array.iter(fun (os, size) -> wrappedWriter(size); wrappedWriter(os))
    s.Seek(int64 tsOffsetTableStart, SeekOrigin.Begin) |> ignore
    tsOffsets |> Array.iter(fun (os, size) -> wrappedWriter(size); wrappedWriter(os))

/// <summary>
/// Creates a byte stream that represents an MO file, using the specified encoding.
/// </summary>
/// <param name="safi">the contents of the MO file</param>
/// <param name="encoding">the encoding to use</param>
let internal createByteStreamForSAFI(safi: StringsAndFileInfo, encoding: Encoding) = 
    let ms = new MemoryStream()
    writeByteStreamForSAFI(ms, safi, encoding)
    ms.Seek(int64 0, SeekOrigin.Begin) |> ignore
    ms

/// <summary>
/// Shrinker for MO strings. Shrinks and removes the context, singular string, 
/// and plural forms.
/// </summary>
/// <param name="m">the string to shrink</param>
let rec shrinkMoString(m: MoString) = 
    let contextShrinks(m: MoString) = 
        match m.context with
        | Some(c) -> 
            let shrunk = { m with context = None }
            let lengthShrunk = powerOfTwoShrinks(c.Length) |> Seq.map(fun n -> { m with context = Some(c.Substring(0, n)) })
            seq { yield shrunk; yield! lengthShrunk }
        | _ -> Seq.empty

    let singularShrinks(m: MoString) = 
        powerOfTwoShrinks(m.singular.Length) |> Seq.map(fun n -> { m with singular = m.singular.Substring(0, n) })

    let pluralShrinks(m: MoString) = 
        // shrink size of these strings
        let lengthShrinks = Arb.shrink(m.pluralForms) |> Seq.map(fun pf -> { m with pluralForms = pf })
        let firstShrinks = powerOfTwoShrinks(m.pluralForms.Length) |> Seq.map(fun n -> { m with pluralForms = m.pluralForms |> Array.take n})
        seq { yield! lengthShrinks; yield! firstShrinks }

    seq<MoString> {
        yield! contextShrinks(m)
        yield! singularShrinks(m)
        yield! pluralShrinks(m)
    }

/// <summary>
/// Shrinker for the strings of a MO file. Shrinks the original and translated strings
/// in unison.
/// </summary>
let rec shrinkOATS(oats: OriginalAndTranslatedStrings) = 
    let shrinks = powerOfTwoShrinks(oats.originalStrings.Length) |> Seq.ofList
    shrinks |> Seq.map (fun n -> { oats with originalStrings = oats.originalStrings |> Array.take(n); translatedStrings = oats.translatedStrings |> Array.take(n) })
    // TODO: pass this into string shrinkers?

/// <summary>
/// Shrinker for the contents of a MO file. Returns the following variants:
/// - shrunken/removed header padding
/// - shrunken/removed footer padding
/// - shrunken/removed hash table
/// - shrunken/removed strings
/// </summary>
/// <param name="safi">the MO file contents to shrink</param>
let rec shrinkSAFI(safi: StringsAndFileInfo) = 
    let shrinkingHeaderPadding = 
        let s = shrinkArrayLog(safi.headerPadding)
        let mkWithHeader p = { safi with headerPadding = p }
        let withHeaders = s |> Seq.map mkWithHeader
        seq { yield! withHeaders; yield! (withHeaders |> Seq.collect shrinkSAFI) }

    let shrinkingFooterPadding = 
        let s = shrinkArrayLog(safi.footerPadding)
        let mkWithFooter p = { safi with footerPadding = p }
        let withFooters = s |> Seq.map mkWithFooter
        seq { yield! withFooters; yield! (withFooters |> Seq.collect shrinkSAFI) }

    let shrinkingHashTable = 
        let s = shrinkArrayLog(safi.hashTable)
        let mkWithHash p = { safi with hashTable = p }
        let withHash = s |> Seq.map mkWithHash
        seq { yield! withHash; yield! (withHash |> Seq.collect shrinkSAFI) }

    let shrinkingStrings = 
        let s = shrinkOATS(safi.strings)
        let mkWithStrings p = { safi with strings = p }
        let withStrings = s |> Seq.map mkWithStrings
        seq { yield! withStrings; yield! (withStrings |> Seq.collect shrinkSAFI) }

    seq {
        yield! shrinkingHeaderPadding
        yield! shrinkingFooterPadding
        yield! shrinkingHashTable
        yield! shrinkingStrings
    }
