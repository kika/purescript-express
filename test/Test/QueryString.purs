module Test.QueryString (testSuite) where

import Data.Either
import Data.Foldable (for_)
import Data.Maybe
import Node.Express.Internal.QueryString
import Prelude
import Test.Unit
import Test.Unit.Assert

type TestCase a b = { input :: a, expected :: b }

verylong = "side=F&{%22name%22:%22R101%22,%22units%22:42,%22width%22:600,%22depth%22:1200,%22devices%22:[{%22libo%22:%22libo-eaba9443de8c3b6c9f4d891b4cdccd691791435759ef0f030a7b568ffb207ecd%22,%22name%22:%22OPSRV001%22,%22position%22:14,%22side%22:%22F%22},{%22libo%22:%22libo-76998523911836dbfab1cef3d139e3d7%22,%22name%22:%22GW001A%22,%22position%22:38,%22side%22:%22F%22},{%22libo%22:%22libo-76998523911836dbfab1cef3d139e3d7%22,%22name%22:%22GW002A%22,%22position%22:36,%22side%22:%22F%22},{%22libo%22:%22libo-e32b502751baa823e6dde2227963ae9f99dc0531b523b94689b29d199f68cf3f%22,%22name%22:%22FLS001%22,%22position%22:17,%22side%22:%22F%22},{%22libo%22:%22libo-7922f29164294c84416f207fec3bcaa22c662bf784333a9c0d1f9dc1b5e41457%22,%22name%22:%22SP001A%22,%22position%22:20,%22side%22:%22F%22},{%22libo%22:%22libo-31ddf2d9e2a64dc14ab87fc99175c5c4e894991260cde4ab11567100c3bf0d5e%22,%22name%22:%22STOR002%22,%22position%22:21,%22side%22:%22F%22},{%22libo%22:%22libo-d7227691c99dc10372f956de8a934ce7addbea027a96b2dea7b4bde1426c3cbb%22,%22name%22:%22CORE001A%22,%22position%22:31,%22side%22:%22F%22},{%22libo%22:%22libo-91eb86b34b2be6544e46341d7d18340d1ab3c0daf1dcb608988d9187cd413643%22,%22name%22:%22STOR004%22,%22position%22:25,%22side%22:%22F%22},{%22libo%22:%22libo-1c8308dded2b7d7745c34fd645a721b598d94185dab280bd3b2c1c2d717b4e0e%22,%22name%22:%22STOR006%22,%22position%22:29,%22side%22:%22F%22},{%22libo%22:%22libo-828dc1ef7731483acc94a0add9bcec5ff1f34a1e6c551b8913f8daa4c124e464%22,%22name%22:%22SW101A%22,%22position%22:42,%22side%22:%22B%22},{%22libo%22:%22libo-828dc1ef7731483acc94a0add9bcec5ff1f34a1e6c551b8913f8daa4c124e464%22,%22name%22:%22SW101B%22,%22position%22:40,%22side%22:%22B%22},{%22libo%22:%22libo-eaba9443de8c3b6c9f4d891b4cdccd691791435759ef0f030a7b568ffb207ecd%22,%22name%22:%22DB001%22,%22position%22:2,%22side%22:%22F%22},{%22libo%22:%22libo-eaba9443de8c3b6c9f4d891b4cdccd691791435759ef0f030a7b568ffb207ecd%22,%22name%22:%22DB002%22,%22position%22:4,%22side%22:%22F%22},{%22libo%22:%22libo-eaba9443de8c3b6c9f4d891b4cdccd691791435759ef0f030a7b568ffb207ecd%22,%22name%22:%22DB003%22,%22position%22:6,%22side%22:%22F%22},{%22libo%22:%22libo-9af43175c9ba0c65a08673684c4cf46e%22,%22name%22:%22C001%22,%22position%22:8,%22side%22:%22F%22},{%22libo%22:%22libo-9af43175c9ba0c65a08673684c4cf46e%22,%22name%22:%22C002%22,%22position%22:10,%22side%22:%22F%22}]}"

queryParserTestCases =
    [ {input: "a=b",        expected: Right [Param "a" "b"]}
    , {input: "a=b&b=c",    expected: Right [Param "a" "b", Param "b" "c"]}
    , {input: "a[123]=&b=", expected: Right [Param "a[123]" "", Param "b" ""]}
    , {input: "",           expected: Right []}
    , {input: "a=b%20c",    expected: Right [Param "a" "b c"]}
    , {input: "a=b+c",      expected: Right [Param "a" "b c"]}
    , {input: "a=b=c",      expected: Right [Param "a" "b=c"]}
    , {input: "a%3Db=c",    expected: Right [Param "a=b" "c"]}
    , {input: verylong,     expected: Right [Param "side" "F" ]}
    ]

queryGetOneTestCases =
    [ {input: [Param "b" "c"],                  expected: Nothing}
    , {input: [],                               expected: Nothing}
    , {input: [Param "a" "b"],                  expected: Just "b"}
    , {input: [Param "a" "b", Param "a" "c"],   expected: Just "b"}
    ]

queryGetAllTestCases =
    [ {input: [Param "b" "c"],                  expected: []}
    , {input: [],                               expected: []}
    , {input: [Param "a" "b"],                  expected: ["b"]}
    , {input: [Param "a" "b", Param "a" "c"],   expected: ["b", "c"]}
    ]

doTest :: forall a b e. Show b => Eq b => (a -> b) -> TestCase a b -> Test e
doTest fun testCase = do
    let actual = fun testCase.input
        msg = show actual <> " should equal " <> show testCase.expected
    assert msg (actual == testCase.expected)

testSuite =
    suite "QueryString" do
        test "parse" do
            for_ queryParserTestCases $ doTest (\t -> parse t)
        test "getOne" do
            for_ queryGetOneTestCases $ doTest (\t -> getOne t "a")
        test "getAll" do
            for_ queryGetAllTestCases $ doTest (\t -> getAll t "a")
