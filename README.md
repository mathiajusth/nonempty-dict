# Dict.Nonempty 

Similarly to [Nonempty String](https://package.elm-lang.org/packages/MartinSStewart/elm-nonempty-string/latest/) and [Nonempty List](https://package.elm-lang.org/packages/mgold/elm-nonempty-list/latest/) provides
a type that is guaranteed to have at least one entry. 
It consist of a key-value pair and a regular Dict `( ( comparable, v ), Dict comparable v )` hence it only allows comparable keys (same as Dict) and it exposes function `head : NonemptyDict comparable v -> ( comparable, v )` that returns the key-value pair with the lowest key. 
Other functions work the same way as with Dict with sensible exceptions (e.g. `remove : comparable -> NonemptyDict comparable v -> Maybe (NonemptyDict comparable v)` returning Maybe) so the usage is almost the same.

## Why would you use this?

Not sure about that but I am using it for tracking errors when constructing a record out of user input (form) where key is field index. So it is something like a nonempty `Array (Maybe error)` but you don't have to deal with inserting the Nothings as Arrays can't have holes i.e.

```elm
Array.fromList ["a","b"]
  |> Array.set "d"            -- there is gap between last index (1 - where "b" is) and index we are trying to set
  |> Array.toList ["a","b"]   -- hence the the "d" is lost
```

The construction fails only when there is an error so there is a guarantee that there will be at least one entry
and it should be possible to ask - give me only the first error that occurred (hence the above-mentioned function `head`)

