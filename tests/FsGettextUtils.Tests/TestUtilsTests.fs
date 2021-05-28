module TestUtilsTests

open Expecto
open FsCheck
open FsGettextUtils.MoFile
open System
open System.IO
open System.Text
open TestGenerators
open TestUtils

let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
let config50 = { FsCheckConfig.defaultConfig with maxTest = 50 }

let tests = 
  testList "test utils" [
    testPropertyWithConfig config10k "Ensure that shrinkArrayLog can produce the empty array for non-empty input" <|
      fun a -> match a with | NonEmptyArray(b) -> shrinkArrayLog(b) |> Seq.contains(Array.empty)

    testCase "Ensure that shrinkArrayLog returns an empty sequence for an empty array" <|
      fun () -> Expect.isTrue (shrinkArrayLog([||]) |> Seq.isEmpty) "empty array shrinks to an empty sequence"

    testPropertyWithConfig config10k "Ensure that getStringBytes is reversible" <|
      fun () ->
        let test(e: Encoding, m: MoString) = 
          let stringBytes = getStringBytes(e)(m)
          use ms = new MemoryStream(stringBytes)
          let readBack = readString(ms, e)(uint32 stringBytes.Length, uint32 0)
          readBack = m
            
        let g = gen {
          let! encoding = encodingGen
          let! moString = Gen.oneof([| emptyMoStringGen; originalStringGen(nonEmptyMoStringGen) |])
          return (encoding, moString)
        }
        let shrinker(e: Encoding, m: MoString) = 
          shrinkMoString(m) |> Seq.map(fun sm -> (e, sm))

        Prop.forAll(Arb.fromGenShrink(g, shrinker)) test
  ]
