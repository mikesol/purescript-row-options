# purescript-row-options

Like [`purescript-options`](https://pursuit.purescript.org/packages/purescript-options), but uses rows.

Like a variant, but with potentially more or less stuff.

Like a row, but with potentially less stuff.

## How it works

```purescript
type FullRow = (foo :: Int, bar :: String, baz :: Boolean)

ro = options { foo: 1, baz: false } :: RowOptions FullRow
gfoo = get (Proxy :: _ "foo") ro -- Just 1
gbar = get (Proxy :: _ "bar") ro -- Nothing
sfoo = set (Proxy :: _ "foo") 2 ro -- options { foo: 2, baz: false }
sbar = set (Proxy :: _ "bar") "hi" ro -- options { foo: 2, bar: "hi", baz: false }
mbar = modify (Proxy :: _ "bar") "hi" ro -- options { foo: 2, baz: false }
mmp = megamap ro { foo: show :: Int -> _, bar: identity :: String -> _, baz: show :: Boolean -> _ } -- options { foo: "2", baz: "false" }
h = Array.fromFoldable $ homogeneous $ megamap ro { foo: show :: Int -> _, bar: identity :: String -> _, baz: show :: Boolean -> _ } -- ["2", "false"]
```