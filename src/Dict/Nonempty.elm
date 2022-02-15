module Dict.Nonempty exposing
    ( NonemptyDict
    , singleton, fromNonemptyList, fromList
    , insert, remove
    , toDict, toList, toNonemptyList
    , head
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

@docs get, head, headPair, headKey

-}

import Dict exposing (Dict)
import List.Extra as List
import List.Nonempty as NonemptyList


{-| A dict with at least one entry.
-}
type NonemptyDict comparable v
    = NonemptyDict ( ( comparable, v ), Dict comparable v )



---- CONSTRUCTION


{-| Initiate a NonemptyDict out of k v pair
-}
singleton : comparable -> v -> NonemptyDict comparable v
singleton headK headV =
    NonemptyDict
        ( ( headK, headV ), Dict.empty )


{-| Create a NonemptyDict out of a NonemptyList
-}
fromNonemptyList : NonemptyList.Nonempty ( comparable, v ) -> NonemptyDict comparable v
fromNonemptyList (NonemptyList.Nonempty head_ tail) =
    NonemptyDict
        ( head_, Dict.fromList tail )


{-| Create a NonemptyDict out of k v pair and a List of k v pairs.
If there is a key clas it prefers the value provided explicitly in the first argument.
-}
fromList : ( comparable, v ) -> List ( comparable, v ) -> NonemptyDict comparable v
fromList pair list =
    case NonemptyList.fromList list of
        Nothing ->
            NonemptyDict
                ( pair, Dict.empty )

        Just (NonemptyList.Nonempty listHead listTail) ->
            if List.member pair list then
                NonemptyDict
                    ( listHead, Dict.fromList listTail )

            else
                NonemptyDict
                    ( pair
                    , Dict.fromList list
                        -- remove the value from dict if there is key clash
                        |> Dict.remove (Tuple.first pair)
                    )



---- MEMBER CONTROL


{-| Same as Dict.insert
-}
insert : comparable -> v -> NonemptyDict comparable v -> NonemptyDict comparable v
insert newK newV (NonemptyDict ( ( headK, headV ), dictTail )) =
    if newK == headK then
        NonemptyDict
            ( ( newK, newV ), dictTail )

    else
        NonemptyDict
            ( ( headK, headV ), Dict.insert newK newV dictTail )


{-| Same as Dict.remove but fails with Nothing if you remove the only entry that was left
-}
remove : comparable -> NonemptyDict comparable v -> Maybe (NonemptyDict comparable v)
remove k (NonemptyDict ( ( headK, headV ), dictTail )) =
    if k == headK then
        dictTail
            |> Dict.toList
            |> List.uncons
            |> (Maybe.map << Tuple.mapSecond) Dict.fromList
            |> Maybe.map NonemptyDict

    else
        ( ( headK, headV ), Dict.remove k dictTail )
            |> NonemptyDict
            |> Just



---- TRANSFORMATION


{-| Transform NonemptyDict into regular Dict
-}
toDict : NonemptyDict comparable v -> Dict comparable v
toDict (NonemptyDict ( ( headK, headV ), dictTail )) =
    Dict.insert headK headV dictTail


{-| Same as Dict.toList
-}
toList : NonemptyDict comparable v -> List ( comparable, v )
toList (NonemptyDict ( headPair, dictTail )) =
    headPair :: Dict.toList dictTail


{-| Transform NonemptyDict into List.Nonempty.Nonempty
-}
toNonemptyList : NonemptyDict comparable v -> NonemptyList.Nonempty ( comparable, v )
toNonemptyList (NonemptyDict ( headPair, dictTail )) =
    NonemptyList.Nonempty headPair (Dict.toList dictTail)



---- DESTRUCTION (loss of information as opposed to Transformation)


{-| Get the key value pair corresponding to the lowest key
-}
head : NonemptyDict comparable v -> ( comparable, v )
head (NonemptyDict ( ( headK, headV ), dictTail )) =
    case List.head (Dict.toList dictTail) of
        Just ( tailsHeadK, tailsHeadV ) ->
            if headK <= tailsHeadK then
                ( headK, headV )

            else
                ( tailsHeadK, tailsHeadV )

        Nothing ->
            ( headK, headV )


{-| Same as Dict.get
-}
get : comparable -> NonemptyDict comparable v -> Maybe v
get k (NonemptyDict ( ( headK, headV ), dictTail )) =
    if k == headK then
        Just headV

    else
        Dict.get k dictTail
