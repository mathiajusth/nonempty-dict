module Dict.Nonempty exposing
    ( NonemptyDict
    , singleton, fromNonemptyList, fromList
    , insert, remove
    , toDict, toList, toNonemptyList
    , get, head
    )

{-| A dict that is guaranteed to have an entry in it.


# Type

@docs NonemptyDict


# Construction

@docs singleton, fromNonemptyList, fromList


# Member control

@docs insert, remove


# Transformation

@docs toDict, toList, toNonemptyList


# Destruction

@docs get, head

-}

import Dict exposing (Dict)
import List.Extra as List
import List.Nonempty as NonemptyList


{-| A dict with at least one entry.
The inner dict always contains all of the information. The inner tuple is a redundancy
to ensure the nonemptiness
-}
type NonemptyDict comparable v
    = NonemptyDict ( ( comparable, v ), Dict comparable v )



---- CONSTRUCTION


{-| Initiate a NonemptyDict out of k v pair
-}
singleton : comparable -> v -> NonemptyDict comparable v
singleton guaranteedK guaranteedV =
    NonemptyDict
        ( ( guaranteedK, guaranteedV ), Dict.singleton guaranteedK guaranteedV )


{-| Create a NonemptyDict out of a NonemptyList.
Prefers last value if keys clash.
-}
fromNonemptyList : NonemptyList.Nonempty ( comparable, v ) -> NonemptyDict comparable v
fromNonemptyList (NonemptyList.Nonempty ( guaranteedK, guaranteedV ) tail) =
    NonemptyDict
        (let
            dict : Dict comparable v
            dict =
                Dict.fromList (( guaranteedK, guaranteedV ) :: tail)
         in
         ( ( guaranteedK
           , -- this is to ensure the same behaviour as in Dict.fromList i.e. the last entry is prefered when keys clash
             Maybe.withDefault guaranteedV (Dict.get guaranteedK dict)
           )
         , dict
         )
        )


{-| Create a NonemptyDict out of k v pair and a List of k v pairs.
Prefers value provided in the first argument if keys clash
-}
fromList : ( comparable, v ) -> List ( comparable, v ) -> NonemptyDict comparable v
fromList ( guaranteedK, guaranteedV ) list =
    NonemptyDict
        ( ( guaranteedK, guaranteedV )
        , Dict.fromList (( guaranteedK, guaranteedV ) :: list)
            |> Dict.insert guaranteedK guaranteedV
        )



---- MEMBER CONTROL


{-| Same as Dict.insert
-}
insert : comparable -> v -> NonemptyDict comparable v -> NonemptyDict comparable v
insert newK newV (NonemptyDict ( ( guaranteedK, guaranteedV ), dictTail )) =
    NonemptyDict
        ( ( guaranteedK
          , if guaranteedK == newK then
                newV

            else
                guaranteedV
          )
        , dictTail
            |> Dict.insert newK newV
        )


{-| Same as Dict.remove but fails with Nothing if you remove the only entry that was left
-}
remove : comparable -> NonemptyDict comparable v -> Maybe (NonemptyDict comparable v)
remove k (NonemptyDict ( ( guaranteedK, guaranteedV ), dictTail )) =
    let
        newDict : Dict comparable v
        newDict =
            dictTail
                |> Dict.remove k
    in
    if k /= guaranteedK then
        Just (NonemptyDict ( ( guaranteedK, guaranteedV ), newDict ))

    else
        newDict
            |> Dict.toList
            |> List.head
            |> Maybe.map
                (\newHead ->
                    NonemptyDict
                        ( newHead
                        , newDict
                        )
                )



---- TRANSFORMATION


{-| Transform NonemptyDict into regular Dict
-}
toDict : NonemptyDict comparable v -> Dict comparable v
toDict (NonemptyDict ( _, dictTail )) =
    dictTail


{-| Same as Dict.toList
-}
toList : NonemptyDict comparable v -> List ( comparable, v )
toList (NonemptyDict ( _, dictTail )) =
    Dict.toList dictTail


{-| Transform NonemptyDict into List.Nonempty.Nonempty
-}
toNonemptyList : NonemptyDict comparable v -> NonemptyList.Nonempty ( comparable, v )
toNonemptyList (NonemptyDict ( guarantee, dictTail )) =
    dictTail
        |> Dict.toList
        |> NonemptyList.fromList
        |> Maybe.withDefault (NonemptyList.singleton guarantee)



---- DESTRUCTION (loss of information as opposed to Transformation)


{-| Get the key value pair corresponding to the lowest key
-}
head : NonemptyDict comparable v -> ( comparable, v )
head (NonemptyDict ( guarantee, dictTail )) =
    dictTail
        |> Dict.toList
        |> List.head
        |> Maybe.withDefault guarantee


{-| Same as Dict.get
-}
get : comparable -> NonemptyDict comparable v -> Maybe v
get k (NonemptyDict ( _, dictTail )) =
    Dict.get k dictTail
