module Example exposing (suite)

import Dict
import Dict.Nonempty as NonemptyDict exposing (NonemptyDict)
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import List.Extra as List
import List.Nonempty as NonemptyList
import Test exposing (Test)


fuzzPair : Fuzzer ( Int, String )
fuzzPair =
    Fuzz.tuple ( Fuzz.int, Fuzz.string )


fuzzPairs : Fuzzer (List ( Int, String ))
fuzzPairs =
    Fuzz.list <|
        Fuzz.tuple ( Fuzz.int, Fuzz.string )


fuzzNonemptyPairs : Fuzzer (NonemptyList.Nonempty ( Int, String ))
fuzzNonemptyPairs =
    Fuzz.map2 NonemptyList.Nonempty
        fuzzPair
        fuzzPairs


equalNonemptyDicts : NonemptyDict comparable v -> NonemptyDict comparable v -> Expectation
equalNonemptyDicts nonemptyDictA nonemptyDictB =
    Expect.true "NonemptyDict.equal" (NonemptyDict.equal nonemptyDictA nonemptyDictB)


suite : Test
suite =
    Test.describe "Dict.Nonempty test suite"
        [ Test.describe "singleton"
            [ Test.fuzz2
                Fuzz.int
                Fuzz.string
                "works with get"
                (\key value ->
                    NonemptyDict.singleton key value
                        |> NonemptyDict.get key
                        |> Expect.equal (Just value)
                )
            ]
        , Test.describe "fromNonemptyList"
            [ Test.fuzz3
                Fuzz.int
                Fuzz.string
                Fuzz.string
                "prefers last value if keys clash"
                (\key valueA valueB ->
                    NonemptyList.Nonempty ( key, valueA ) [ ( key, valueB ) ]
                        |> NonemptyDict.fromNonemptyList
                        |> NonemptyDict.get key
                        |> Expect.equal (Just valueB)
                )
            , Test.fuzz
                fuzzNonemptyPairs
                "is inverse to toNonemptyList"
                (\nonemptyList ->
                    let
                        nonemptyDict =
                            NonemptyDict.fromNonemptyList nonemptyList
                    in
                    nonemptyDict
                        |> NonemptyDict.toNonemptyList
                        |> NonemptyDict.fromNonemptyList
                        |> equalNonemptyDicts nonemptyDict
                )
            ]
        , Test.describe "fromList"
            [ Test.fuzz3
                Fuzz.int
                Fuzz.string
                Fuzz.string
                "prefers value provided in the first argument if keys clash"
                (\key valueA valueB ->
                    NonemptyDict.fromList ( key, valueA ) [ ( key, valueB ) ]
                        |> NonemptyDict.get key
                        |> Expect.equal (Just valueA)
                )
            , Test.fuzz2
                fuzzPair
                fuzzPairs
                " (fromList (k,v) >> toList) == (Dict.fromList >> Dict.insert k v >> Dict.toList)"
                (\( k, v ) pairList ->
                    pairList
                        |> NonemptyDict.fromList ( k, v )
                        |> NonemptyDict.toList
                        |> Expect.equal
                            (pairList
                                |> Dict.fromList
                                |> Dict.insert k v
                                |> Dict.toList
                            )
                )
            ]
        , Test.describe "insert"
            [ Test.fuzz2
                Fuzz.string
                fuzzNonemptyPairs
                "works with get"
                (\newValue ((NonemptyList.Nonempty ( keyToChange, _ ) _) as nonemptyList) ->
                    NonemptyDict.fromNonemptyList nonemptyList
                        |> NonemptyDict.insert keyToChange newValue
                        |> NonemptyDict.get keyToChange
                        |> Expect.equal (Just newValue)
                )
            , Test.fuzz3
                Fuzz.string
                fuzzPair
                fuzzPairs
                "overwrites old value if keys clash"
                (\newValue ( key, value ) list ->
                    let
                        nonemptyList : NonemptyList.Nonempty ( Int, String )
                        nonemptyList =
                            NonemptyList.Nonempty ( key, value ) list
                    in
                    NonemptyDict.fromNonemptyList nonemptyList
                        |> NonemptyDict.insert key newValue
                        |> NonemptyDict.get key
                        |> Expect.equal (Just newValue)
                )
            ]
        , Test.describe "remove"
            [ Test.fuzz
                fuzzPair
                "destroys singletons"
                (\( key, value ) ->
                    NonemptyDict.singleton key value
                        |> NonemptyDict.remove key
                        |> Expect.equal Nothing
                )
            , Test.fuzz2
                Fuzz.string
                fuzzNonemptyPairs
                "removes"
                (\newValue ((NonemptyList.Nonempty ( keyToRemove, _ ) _) as nonemptyList) ->
                    NonemptyDict.fromNonemptyList nonemptyList
                        |> NonemptyDict.remove keyToRemove
                        |> Maybe.andThen (NonemptyDict.get keyToRemove)
                        |> Expect.equal Nothing
                )
            ]
        , Test.describe "toDict"
            [ Test.fuzz
                fuzzNonemptyPairs
                "preserver the key-value pairs correctly"
                (\nonemptyList ->
                    NonemptyDict.fromNonemptyList nonemptyList
                        |> NonemptyDict.toDict
                        |> Expect.equal
                            (nonemptyList
                                |> NonemptyList.toList
                                |> Dict.fromList
                            )
                )
            ]
        , Test.describe "toNonemptyList"
            [ Test.fuzz
                fuzzNonemptyPairs
                "is inverse to fromNonemptyList up to uniqueness and sorting it based on key"
                (\nonemptyList ->
                    NonemptyDict.fromNonemptyList nonemptyList
                        |> NonemptyDict.toNonemptyList
                        -- we compare the result on List level as uniqueBy is
                        -- available in List.Extra but not List.Nonempty
                        |> NonemptyList.toList
                        |> Expect.equal
                            (nonemptyList
                                |> NonemptyList.toList
                                |> List.reverse
                                |> List.uniqueBy Tuple.first
                                |> List.sortBy Tuple.first
                            )
                )
            ]
        , Test.describe "head"
            [ Test.fuzz
                fuzzNonemptyPairs
                "returns the pair corresponding to the lowest key"
                (\nonemptyList ->
                    let
                        nonemptyDict =
                            NonemptyDict.fromNonemptyList nonemptyList
                    in
                    nonemptyDict
                        |> NonemptyDict.head
                        |> Expect.equal
                            (nonemptyDict
                                |> NonemptyDict.toNonemptyList
                                |> NonemptyList.sortBy Tuple.first
                                |> NonemptyList.head
                            )
                )
            ]
        ]
