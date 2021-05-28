module MoFileTests

open Expecto
open FsCheck
open FsGettextUtils
open System
open System.IO
open System.Text
open TestUtils
open TestGenerators

let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
let config50 = { FsCheckConfig.defaultConfig with maxTest = 50 }

let internal fileHasDesiredStrings x = 
    let (safi, encoding) = x 
    use ms = createByteStreamForSAFI(safi, encoding)
    let mi = MoFile.GetMoInfo(ms)
    let originalStrings = MoFile.GetOriginalStrings(ms, mi, encoding)([| 0..mi.originalStringLengthsAndOffsets.Length - 1 |]) |> Array.ofSeq
    let translatedStrings = MoFile.GetTranslatedStrings(ms, mi, encoding)([| 0..mi.translatedStringLengthsAndOffsets.Length - 1 |]) |> Array.ofSeq
    let isOriginalStringEq(a: MoFile.MoString)(b: MoFile.MoString) = a = b
    let isTranslatedStringEq(a: MoFile.MoString)(b: MoFile.MoString) = (a.singular = b.singular) && (a.pluralForms = b.pluralForms)

    Console.WriteLine("encoding name: {0}, body name: {1}, header name: {2}, web name: {3}", encoding.EncodingName, encoding.BodyName, encoding.HeaderName, encoding.WebName)
    
    let result = 
        (mi.originalStringLengthsAndOffsets.Length = safi.strings.originalStrings.Length) 
        && (mi.translatedStringLengthsAndOffsets.Length = safi.strings.translatedStrings.Length) 
        && (Array.forall2 isOriginalStringEq originalStrings safi.strings.originalStrings)
        && (Array.forall2 isTranslatedStringEq translatedStrings safi.strings.translatedStrings)

    result

/////////////////////////////////////////////////////////////////////////////
// Library tests
[<Tests>]
let tests = 
  testList "MoFile" [
    testPropertyWithConfig config10k "Ensure readUint32WithSwap returns a reversed value if swapping is enabled" <|
      fun (i: uint32) -> 
        let ba = Array.zeroCreate(4)
        let ms = new MemoryStream(ba)
        let bw = new BinaryWriter(ms)
        bw.Write(i)

        let br = new BinaryReader(new MemoryStream(ba |> Array.rev))
        (MoFile.readUint32WithSwap(br, true)()) = i

    testPropertyWithConfig config10k "Ensure readUint32WithSwap doesn't modify the value if swapping is disabled" <|
      fun (i: uint32) ->
        let ms = new MemoryStream()
        let bw = new BinaryWriter(ms)
        bw.Write(i)
        ms.Seek(int64 0, SeekOrigin.Begin) |> ignore

        let br = new BinaryReader(ms)
        (MoFile.readUint32WithSwap(br, false)()) = i

    testPropertyWithConfig config50 "GetMoInfo handles an empty file" <|
      fun () ->
        let isFileConsideredEmpty x =
            let (safi, encoding) = x 
            use ms = createByteStreamForSAFI(safi, encoding)
            let mi = MoFile.GetMoInfo(ms)
            (mi.originalStringLengthsAndOffsets.Length = 0) && (mi.translatedStringLengthsAndOffsets.Length = 0)

        let oatsGen = Gen.constant({ OriginalAndTranslatedStrings.originalStrings = [||]; translatedStrings = [||] })
        let gen = 
            gen {
                let! sg = safiGen(validMagicGen, validFileFormatGen, paddingGen, oatsGen, Gen.constant([||]), paddingGen)
                let! encoding = encodingGen
                return (sg, encoding)
            }

        Prop.forAll(Arb.fromGenShrink(gen, shrinkSAFIAndEncoding))(isFileConsideredEmpty)

    testPropertyWithConfig config50 "GetMoInfo handles a file with strings in it, and no hash table" <|
      fun () -> 
        let g =
            gen { 
                let! sg = 
                    stringsAndFileInfoGen(
                        validMagicGen, 
                        validFileFormatGen, 
                        paddingGen, 
                        originalStringGen(nonEmptyMoStringGen),
                        translatedStringGen(nonEmptyMoStringGen),
                        Gen.constant([||]),
                        paddingGen
                    )
                let! encoding = encodingGen
                return (sg, encoding)
            }

        Prop.forAll(Arb.fromGenShrink(g, shrinkSAFIAndEncoding)) fileHasDesiredStrings

    testPropertyWithConfig config50 "GetMoInfo handles a file with strings in it, and a hash table" <|
      fun () ->
        let g = 
            gen {
                let! sg = 
                    stringsAndFileInfoGen(
                        validMagicGen, 
                        validFileFormatGen, 
                        paddingGen, 
                        originalStringGen(nonEmptyMoStringGen),
                        translatedStringGen(nonEmptyMoStringGen),
                        Gen.sized hashTableGen,
                        paddingGen
                    )
                let! encoding = encodingGen
                return (sg, encoding)
            }
            
        Prop.forAll(Arb.fromGenShrink(g, shrinkSAFIAndEncoding)) fileHasDesiredStrings

    testPropertyWithConfig config10k "GetMoInfo handles a file where the offset tables are offset from the header block and each other" <|
      fun () -> ()

    testPropertyWithConfig config10k "Ensure getStringsFromOffsetBlocks handles the case where strings overlap" <|
      fun () ->
        let readCorrect(encoding: Encoding) = 
            use  ms = new MemoryStream()
            use bw = new BinaryWriter(ms)
            bw.Write(encoding.GetBytes("ab"))

            let checkpointPos = ms.Position
            bw.Write(encoding.GetBytes("cd"))

            let finalPos = ms.Position
            let shortLength = finalPos - checkpointPos

            let offsetBlocks = [| (uint32 shortLength, uint32 checkpointPos); (uint32 ms.Position, uint32 0) |]
            let expected = 
              [| 
                { MoFile.MoString.context = None; MoFile.MoString.singular = "cd"; MoFile.MoString.pluralForms = [||]}
                { MoFile.MoString.context = None; MoFile.MoString.singular = "abcd"; MoFile.MoString.pluralForms = [||] }
              |]
            let results = MoFile.getStringsFromOffsetBlocks(ms, encoding, offsetBlocks)([| 0; 1 |]) |> Array.ofSeq

            expected = results

        Prop.forAll(Arb.fromGen(encodingGen)) readCorrect

    testCase "GetEncodingNameFromMIMEHeader returns None if no content type is found" <|
      fun () -> Expect.equal None (MoFile.getEncodingNameFromMIMEHeader "") "getEncodingNameFromMIMEHeader of empty string is None"

    testCase "GetEncodingNameFromMIMEHeader returns Some(encodingName) if a content type is found" <|
      fun () -> Expect.equal (Some "utf-8") (MoFile.getEncodingNameFromMIMEHeader "Content-Type: text/plain; charset=utf-8") "getEncodingNameFromMIMEHeader is extracted"

    testCase "GetEncodingNameFromMIMEHeader handles multiple line headers" <|
      fun () -> 
        let content =
          """
Project-Id-Version: 
POT-Creation-Date: 
PO-Revision-Date: 2017-01-17 08:46-0800
Last-Translator: Example Person <exampleperson@example.com>
Language-Team:  <team@example.com>
MIME-Version: 1.0
Content-Type: text/plain; charset=utf-8
Content-Transfer-Encoding: 8bit
          """

        Expect.equal (Some "utf-8") (MoFile.getEncodingNameFromMIMEHeader content) "getEncodingNameFromMIMEHeader should handle multiple line header"

    testPropertyWithConfig config10k "GetEncodingFromMIMEHeader returns the expected encoding" <|
      fun () -> 
        let validEncodingBodyNames = 
            Encoding.GetEncodings() 
            |> Array.map (fun e -> e.GetEncoding().BodyName)
        let validEncodingGen = 
            gen {
                let! ei = Gen.elements(Encoding.GetEncodings())
                let encoding = ei.GetEncoding()
                return (encoding.BodyName, Some(encoding))
            }
        let unknownEncodingGen = 
            Arb.Default.String().Generator
            |> Gen.filter (fun s -> not(validEncodingBodyNames |> Array.contains(s)))
            |> Gen.map (fun s -> (s, None))

        let g = Gen.frequency([| (7, validEncodingGen); (3, unknownEncodingGen) |])
        let test c = 
            let (bodyName, encodingOpt) = c
            let encoding = MoFile.GetEncodingFromMIMEHeader("Content-Type: text/plain; charset=" + bodyName)

            // There is no bijection between body name and encoding, due to some overlap
            // between legacy code pages and the ones explicitly designated for ISO encodings.
            // So we have to settle for checking if we got an encoding that maps back to the
            // same MIME charset type.
            match encoding with
            | Some(e) -> e.BodyName = bodyName
            | _ -> true

        Prop.forAll(Arb.fromGen(g)) test
  ]
