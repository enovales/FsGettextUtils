module TestGenerators

open FsCheck
open FsGettextUtils
open System
open System.Text
open TestUtils

#if NETCOREAPP2_0 || NETCOREAPP2_1 || NETCOREAPP2_2
Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
#endif

/// <summary>
/// Removes characters from a string that would be invalid when encoded as part
/// of an MO file string. This includes the context separator (4) and
/// embedded nulls.
/// </summary>
/// <param name="s">the string to sanitize</param>
let private stripInvalidChars(s: string) = 
    match s with
    | null -> s
    | _ ->
        let invalid = [| char 4; char 0 |]
        let invalidStrings = invalid |> Array.map(fun c -> new String(c, 1))
        invalidStrings |> Array.fold(fun(newString: string)(invalidString: string) -> newString.Replace(invalidString, "")) s

/// <summary>
/// Generator for valid MO file magic numbers.
/// </summary>
let internal validMagicGen = Gen.elements(validMagicNumbers)

/// <summary>
/// Generator for invalid MO file magic numbers.
/// </summary>
let internal invalidMagicGen = Arb.Default.UInt32().Generator |> Gen.filter(fun u -> not(validMagicNumbers |> Array.contains(u)))

/// <summary>
/// Generator for string contexts -- an optional string.
/// </summary>
let internal contextGen = Gen.oneof([| Gen.constant(None); Arb.Default.String().Generator |> Gen.map Some |])

/// <summary>
/// Generates MO strings, consisting of an optional context, a singular form, and plural forms.
/// </summary>
/// <param name="contextGen">generator used to create the optional context</param>
/// <param name="singularGen">generator used for the singular form of the string</param>
/// <param name="pluralFormsGen">generator used for the plural forms of the string</param>
let internal moStringGen(contextGen: Gen<string option>, singularGen: Gen<string>, pluralFormsGen: Gen<string array>) = 
    gen {
        let! context = contextGen
        let! singular = singularGen
        let! pluralForms = pluralFormsGen

        return { MoString.context = context; singular = singular; pluralForms = pluralForms }
    }

/// <summary>
/// Generator that always returns a MO string with no context, no plural forms, and an empty singular form.
/// </summary>
let emptyMoStringGen = 
    Gen.constant({ MoString.context = None; singular = ""; pluralForms = Array.empty })

/// <summary>
/// Generator that returns a MO string with varying contents in each field. The 
/// string values are all sanitized with stripInvalidChars.
/// </summary>
let nonEmptyMoStringGen = 
    moStringGen(
        Arb.Default.StringWithoutNullChars().Generator |> Gen.map string |> Gen.map stripInvalidChars |> Gen.map Option.ofObj,
        Arb.Default.StringWithoutNullChars().Generator |> Gen.map string |> Gen.filter(isNull >> not) |> Gen.map stripInvalidChars, 
        Gen.arrayOf(Arb.Default.StringWithoutNullChars().Generator |> Gen.map string |> Gen.filter(isNull >> not) |> Gen.map stripInvalidChars)
    )

/// <summary>
/// Pass-through generator, intended to denote that the value is intended to be
/// used as an "original string" in the MO file.
/// </summary>
let originalStringGen(g: Gen<MoString>) = g

/// <summary>
/// Wrapper for MoString generators which filters out the context field. This
/// makes it usable as a "translated string" in the MO file.
/// </summary>
let translatedStringGen(g: Gen<MoString>) = g |> Gen.filter(fun m -> m.context.IsNone)

/// <summary>
/// Generator that emits valid, understood file format versions.
/// </summary>
let internal validFileFormatGen = Gen.elements(validFileFormatVersions)

/// <summary>
/// Generator that emits invalid file format versions which cannot be read by this
/// library.
/// </summary>
let internal invalidFileFormatGen = Arb.Default.UInt32().Generator |> Gen.filter(fun u -> not(validFileFormatVersions |> Array.contains(u)))

/// <summary>
/// Generator for creating optional padding bytes for the MO file.
/// This can be placed in various places throughout the file, such as
/// after the header, or after the string data.
/// </summary>
let internal paddingGen: Gen<byte array> = 
    Gen.frequency
        [| 
            (9, Gen.constant([||]))
            (1, Gen.choose(1, 1 * 1024 * 1024) |> Gen.map Array.zeroCreate)
        |]

/// <summary>
/// Generator for creating fake hash table data for an MO file.
/// </summary>
/// <param name="numStrings">the number of strings in the file</param>
let internal hashTableGen(numStrings: int) = 
    Gen.frequency
        [|
            (7, Gen.constant([||]))
            (3, Arb.Default.UInt32().Generator |> Gen.arrayOfLength(numStrings))
        |]

/// <summary>
/// Generator for the sets of original and translated strings, which creates
/// output with: 1) the same number of original and translated strings, and 
/// 2) fewer translated strings than original strings.
/// </summary>
/// <param name="originalStringGen">generator for original strings</param>
/// <param name="translatedStringGen">generator for translated strings</param>
let private originalAndTranslatedStringsGen(originalStringGen: Gen<MoString>, translatedStringGen: Gen<MoString>) = 
    let stringsOfSizeGen n = 
        gen { 
            let! size = Gen.choose(0, n + 1)
            let! os = Gen.arrayOfLength size originalStringGen
            let! ts = Gen.arrayOfLength size translatedStringGen
            return (os, ts)
        }

    gen {
        let! (os, ts) = Gen.sized <| stringsOfSizeGen
        return 
            {
                OriginalAndTranslatedStrings.originalStrings = os
                translatedStrings = ts
            }
    }

/// <summary>
/// Generator for creating the contents of an MO file. This variant uses a single
/// generator for the block of original and translated strings, allowing for
/// exact control over its size and contents.
/// </summary>
/// <param name="magicGen">generator used for magic number in header, and endianness</param>
/// <param name="fileFormatVersionGen">generator used for file format version in header</param>
/// <param name="headerPaddingGen">generator used for padding after the header block</param>
/// <param name="oatsGen">generator for the block of original and translated strings</param>
/// <param name="hashTableGen">generator for the hash table data</param>
/// <param name="footerPaddingGen">generator for the padding after the string data</param>
let internal safiGen(magicGen: Gen<uint32>, fileFormatVersionGen: Gen<uint32>, headerPaddingGen: Gen<byte array>, oatsGen: Gen<OriginalAndTranslatedStrings>, hashTableGen: Gen<uint32 array>, footerPaddingGen: Gen<byte array>) = 
    gen {
        let! m = magicGen
        let! ffv = fileFormatVersionGen
        let! hp = headerPaddingGen
        let! oats = oatsGen
        let! ht = hashTableGen
        let! fp = footerPaddingGen
        return
            {
                StringsAndFileInfo.magic = m
                formatVersion = ffv
                headerPadding = hp
                strings = oats
                hashTable = ht
                footerPadding = fp
            }
    }

/// <summary>
/// Generator for creating the contents of an MO file.
/// </summary>
/// <param name="magicGen">generator used for magic number in header, and endianness</param>
/// <param name="fileFormatVersionGen">generator used for file format version in header</param>
/// <param name="headerPaddingGen">generator used for padding after the header block</param>
/// <param name="originalStringGen">generator for original strings</param>
/// <param name="translatedStringGen">generator for translated strings</param>
/// <param name="hashTableGen">generator for the hash table data</param>
/// <param name="footerPaddingGen">generator for the padding after the string data</param>
let internal stringsAndFileInfoGen(magicGen: Gen<uint32>, fileFormatVersionGen: Gen<uint32>, headerPaddingGen: Gen<byte array>, originalStringGen: Gen<MoString>, translatedStringGen: Gen<MoString>, hashTableGen: Gen<uint32 array>, footerPaddingGen: Gen<byte array>) = 
    let oatsGen = originalAndTranslatedStringsGen(originalStringGen, translatedStringGen)
    safiGen(magicGen, fileFormatVersionGen, headerPaddingGen, oatsGen, hashTableGen, footerPaddingGen)

/// <summary>
/// Generator that emits string encodings that can be used with MO file generation.
/// The ReplacementFallback strategy is used for both encoding and decoding.
/// </summary>    
let internal encodingGen = 
    let allEncodings = Encoding.GetEncodings()
    let disallowedEncodingsByName = 
        [|
            // temporary blacklist
            "iso-2022-kr"
            "x-IA5"
            "x-IA5-German"
            "x-IA5-Norwegian"
            "x-IA5-Swedish"
            "x-mac-thai"
            "IBM420"
            "IBM423"
            "x-Europa"
        |]
        |> Seq.choose(fun en -> allEncodings |> Seq.tryFind(fun e -> e.Name = en))
        |> Seq.map(fun ei -> ei.GetEncoding())

    let disallowedEncodingsByCodePage = 
        [|
            50220 // "iso-2022-jp"
            50221 // "csISO2022JP" allow 1 byte kana
            50222 // "iso-2022-jp" allow 1 byte kana so/si
            20269 // code page 20269/ISO-6937
            20261 // code page 20261/T.61
        |]
        |> Seq.choose(fun en -> allEncodings |> Seq.tryFind(fun e -> e.CodePage = en))
        |> Seq.map(fun ei -> ei.GetEncoding())

    let disallowedEncodings = 
        Seq.concat([disallowedEncodingsByName; disallowedEncodingsByCodePage])

    Gen.elements(Encoding.GetEncodings()) 
    |> Gen.map (fun ei -> Encoding.GetEncoding(ei.CodePage, EncoderFallback.ReplacementFallback, DecoderFallback.ReplacementFallback)) 
    |> Gen.filter (fun e -> not(disallowedEncodings |> Seq.exists(fun de -> de.CodePage = e.CodePage)))

/// <summary>
/// Shrinker for the tuple of MO file data and string encoding. Just passes the 
/// MO file data to its standard shrinker.
/// </summary>
/// <param name="x">the tuple to shrink</parma>
let internal shrinkSAFIAndEncoding x = 
    let (safi, encoding) = x
    shrinkSAFI(safi) |> Seq.map (fun s -> (s, encoding))
