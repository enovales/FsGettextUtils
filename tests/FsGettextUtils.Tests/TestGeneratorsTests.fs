module TestGeneratorsTests

open Expecto
open FsCheck
open FsGettextUtils
open System
open System.IO
open System.Text
open TestUtils
open TestGenerators

let private isNoneOrNonNullSome(o: 'a option) = o.IsNone || not(isNull o.Value)

let private noNullsMoString(s: MoString) = 
    (isNoneOrNonNullSome s.context) 
    && not(isNull s.pluralForms) 
    && (s.pluralForms |> Array.forall(isNull >> not)) 
    && not(isNull s.singular)

let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
let config50 = { FsCheckConfig.defaultConfig with maxTest = 50 }

let private moFileWithoutHashTableGen = 
    let originalStringGen = originalStringGen(nonEmptyMoStringGen)
    let translatedStringGen = translatedStringGen(nonEmptyMoStringGen)
    gen {
        let! mf = TestGenerators.stringsAndFileInfoGen(validMagicGen, validFileFormatGen, paddingGen, originalStringGen, translatedStringGen, Gen.constant([||]), paddingGen)
        let! encoding = TestGenerators.encodingGen
        return (mf, encoding)
    }


[<Tests>]
let tests = 
  testList "TestGenerators" [
    testPropertyWithConfig config10k "nonEmptyMoStringGen doesn't generate anything with nulls" <|
      fun () -> Prop.forAll(Arb.fromGenShrink(nonEmptyMoStringGen, shrinkMoString)) noNullsMoString

    testPropertyWithConfig config10k "originalStringGen doesn't generate anything with nulls" <|
      fun () -> Prop.forAll(Arb.fromGenShrink(originalStringGen(nonEmptyMoStringGen), shrinkMoString)) noNullsMoString
    
    testPropertyWithConfig config10k "translatedStringGen doesn't generate anything with nulls" <|
      fun () -> Prop.forAll(Arb.fromGenShrink(translatedStringGen(nonEmptyMoStringGen), shrinkMoString)) noNullsMoString

    testCase "Calling readString with a single character singular, and an empty plural form works" <|
      fun () -> 
        let tb = [| byte '*'; 0uy |]
        let ts = readString(new MemoryStream(tb), Encoding.ASCII)(uint32 2, uint32 0)
        let expected = { MoString.context = None; singular = "*"; pluralForms = [| "" |]}
        Expect.equal expected ts "read string has a single character singular, and an empty plural form"

    testPropertyWithConfig config50 "Generated file has the same number of original strings" <|
      fun () ->
        let test x = 
            let (safi, encoding) = x
            use ms = TestUtils.createByteStreamForSAFI(safi, Encoding.ASCII)
            let mi = GetMoInfo(ms)

            mi.originalStringLengthsAndOffsets.Length = safi.strings.originalStrings.Length

        Prop.forAll(Arb.fromGenShrink(moFileWithoutHashTableGen, shrinkSAFIAndEncoding)) test

    testPropertyWithConfig config50 "Generated file has same number of translated strings" <|
      fun () ->
        let test x = 
            let (safi, encoding) = x
            use ms = TestUtils.createByteStreamForSAFI(safi, Encoding.ASCII)
            let mi = GetMoInfo(ms)
            mi.translatedStringLengthsAndOffsets.Length = safi.strings.translatedStrings.Length

        Prop.forAll(Arb.fromGenShrink(moFileWithoutHashTableGen, shrinkSAFIAndEncoding)) test

    testPropertyWithConfig config50 "All offsets inside generated file are within the stream length" <|
      fun () ->
        let test x = 
            let (safi, encoding) = x
            use ms = TestUtils.createByteStreamForSAFI(safi, Encoding.ASCII)
            let mi = GetMoInfo(ms)

            let isWithinStream p = p < uint32 ms.Length

            mi.originalStringLengthsAndOffsets |> Array.forall(snd >> isWithinStream)
            && mi.translatedStringLengthsAndOffsets |> Array.forall(snd >> isWithinStream)

        Prop.forAll(Arb.fromGenShrink(moFileWithoutHashTableGen, shrinkSAFIAndEncoding)) test
  ]
