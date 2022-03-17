module Data.Row.Options
  ( RowOptions
  , options
  , get
  , set
  , modify
  , insert
  , delete
  , rename
  , equal
  , merge
  , union
  , disjointUnion
  , nub
  , Builder
  , b'build
  , b'buildFromScratch
  , b'flip
  , b'insert
  , b'modify
  , b'delete
  , b'rename
  , b'merge
  , b'union
  , b'disjointUnion
  , b'nub
  , class EqualFields
  , equalFields
  , class OrdFields
  , ordFields
  , class ShowFields
  , showFields
  , megamap
  , class MegamapRowOptions
  , class MapRowOptionsWithIndex
  , mapRowOptionsWithIndexBuilder
  , asOptions
  , feelingLucky
  , class RowListKeys
  , rowListKeys
  --
  , unsafeOptionsGet
  ) where

import Prelude

import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..))
import Data.Set (fromFoldable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class MappingWithIndex, ConstMapping(..), mappingWithIndex)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.Row as R
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.RowList as RL
import Record.Unsafe (unsafeDelete, unsafeSet)
import Record.Unsafe.Union (unsafeUnionFn)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data RowOptions (r :: Row Type)

instance eqRowOptions ::
  ( RowToList r rs
  , EqualFields rs r
  ) =>
  Eq (RowOptions r) where
  eq a b = equalFields (Proxy :: Proxy rs) a b

instance ordRowOptions ::
  ( RowToList r rs
  , OrdFields rs r
  , EqualFields rs r
  ) =>
  Ord (RowOptions r) where
  compare a b = ordFields (Proxy :: Proxy rs) a b

instance showRowOptions ::
  ( RowToList r rs
  , ShowFields rs r
  ) =>
  Show (RowOptions r) where
  show a = "RowOptions (" <> showFields (Proxy :: Proxy rs) a <> ")"

options :: forall r1 r2 r3. R.Union r1 r2 r3 => { | r1 } -> RowOptions r3
options = unsafeCoerce

foreign import unsafeOptionsGet :: forall a r. Maybe a -> (a -> Maybe a) -> String -> RowOptions r -> Maybe a

get
  :: forall r r' l a
   . IsSymbol l
  => Cons l a r' r
  => Proxy l
  -> RowOptions r
  -> Maybe a
get = unsafeOptionsGet Nothing Just <<< reflectSymbol

set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => Proxy l
  -> b
  -> RowOptions r1
  -> RowOptions r2
set l b r = unsafeCoerce (unsafeSet (reflectSymbol l) b (unsafeCoerce r))

modify
  :: forall r1 r2 r l a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => Proxy l
  -> (a -> b)
  -> RowOptions r1
  -> RowOptions r2
modify l f r = case get l r of
  Nothing -> unsafeCoerce r
  Just x -> set l (f x) r

insert
  :: forall r1 r2 l a
   . IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => Proxy l
  -> a
  -> RowOptions r1
  -> RowOptions r2
insert l a r = unsafeCoerce (unsafeSet (reflectSymbol l) a (unsafeCoerce r))

delete
  :: forall r1 r2 l a
   . IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => Proxy l
  -> RowOptions r2
  -> RowOptions r1
delete l r = unsafeCoerce (unsafeDelete (reflectSymbol l) (unsafeCoerce r))

rename
  :: forall prev next ty input inter output
   . IsSymbol prev
  => IsSymbol next
  => Cons prev ty inter input
  => Lacks prev inter
  => Cons next ty inter output
  => Lacks next inter
  => Proxy prev
  -> Proxy next
  -> RowOptions input
  -> RowOptions output
rename prev next rowOptions =
  case get prev rowOptions of
    Nothing -> unsafeCoerce rowOptions
    Just x -> insert next x (delete prev rowOptions :: RowOptions inter)

merge
  :: forall r1 r2 r3 r4
   . Union r1 r2 r3
  => Nub r3 r4
  => RowOptions r1
  -> RowOptions r2
  -> RowOptions r4
merge l r = unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce l) (unsafeCoerce r))

union
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => RowOptions r1
  -> RowOptions r2
  -> RowOptions r3
union l r = unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce l) (unsafeCoerce r))

disjointUnion
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Nub r3 r3
  => RowOptions r1
  -> RowOptions r2
  -> RowOptions r3
disjointUnion l r = unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce l) (unsafeCoerce r))

nub
  :: forall r1 r2
   . Nub r1 r2
  => RowOptions r1
  -> RowOptions r2
nub = unsafeCoerce

equal
  :: forall r rs
   . RowToList r rs
  => EqualFields rs r
  => RowOptions r
  -> RowOptions r
  -> Boolean
equal a b = equalFields (Proxy :: Proxy rs) a b

class EqualFields (rs :: RowList Type) (row :: Row Type) | rs -> row where
  equalFields :: Proxy rs -> RowOptions row -> RowOptions row -> Boolean

instance equalFieldsCons ::
  ( IsSymbol name
  , Eq ty
  , Cons name ty tailRow row
  , EqualFields tail row
  ) =>
  EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && equalRest a b
    where
    get' = get (Proxy :: Proxy name)
    equalRest = equalFields (Proxy :: Proxy tail)

instance equalFieldsNil :: EqualFields Nil row where
  equalFields _ _ _ = true

--

class OrdFields (rs :: RowList Type) (row :: Row Type) | rs -> row where
  ordFields :: Proxy rs -> RowOptions row -> RowOptions row -> Ordering

instance ordFieldsCons ::
  ( IsSymbol name
  , Ord ty
  , Cons name ty tailRow row
  , OrdFields tail row
  ) =>
  OrdFields (Cons name ty tail) row where
  ordFields _ a b = case (get' a) `compare` (get' b) of
    LT -> LT
    GT -> GT
    EQ -> ordFields (Proxy :: Proxy tail) a b
    where
    get' = get (Proxy :: Proxy name)

instance ordFieldsNil :: OrdFields Nil row where
  ordFields _ _ _ = EQ

--
class ShowFields (rs :: RowList Type) (row :: Row Type) | rs -> row where
  showFields :: Proxy rs -> RowOptions row -> String

instance showFieldsCons ::
  ( IsSymbol name
  , Show ty
  , Cons name ty tailRow row
  , ShowFields tail row
  ) =>
  ShowFields (Cons name ty tail) row where
  showFields _ a =
    ( case v' of
        Nothing -> ""
        Just x -> (reflectSymbol (Proxy :: _ name) <> ": " <> show x)
    ) <> showFields (Proxy :: Proxy tail) a
    where
    v' = get (Proxy :: Proxy name) a

instance showFieldsNil :: ShowFields Nil row where
  showFields _ _ = ""

------------------ builder

foreign import bcopyRowOptions :: forall r1. RowOptions r1 -> RowOptions r1
foreign import bunsafeInsert :: forall a r1 r2. String -> a -> RowOptions r1 -> RowOptions r2
foreign import bunsafeModify :: forall a b r1 r2. String -> (a -> b) -> RowOptions r1 -> RowOptions r2
foreign import bunsafeDelete :: forall r1 r2. String -> RowOptions r1 -> RowOptions r2
foreign import bunsafeRename :: forall r1 r2. String -> String -> RowOptions r1 -> RowOptions r2

newtype Builder a b = Builder (a -> b)

b'build :: forall r1 r2. Builder (RowOptions r1) (RowOptions r2) -> RowOptions r1 -> RowOptions r2
b'build (Builder b) r1 = b (bcopyRowOptions r1)

b'buildFromScratch :: forall r. Builder (RowOptions ()) (RowOptions r) -> RowOptions r
b'buildFromScratch = flip b'build (options {})

b'flip :: forall r1 r2 r3. (RowOptions r1 -> Builder (RowOptions r2) (RowOptions r3)) -> RowOptions r2 -> Builder (RowOptions r1) (RowOptions r3)
b'flip f b = Builder \a -> b'build (f a) b

derive newtype instance semigroupoidBuilder :: Semigroupoid Builder
derive newtype instance categoryBuilder :: Category Builder

b'insert
  :: forall l a r1 r2
   . Row.Cons l a r1 r2
  => Row.Lacks l r1
  => IsSymbol l
  => Proxy l
  -> a
  -> Builder (RowOptions r1) (RowOptions r2)
b'insert l a = Builder \r1 -> bunsafeInsert (reflectSymbol l) a r1

b'modify
  :: forall l a b r r1 r2
   . Row.Cons l a r r1
  => Row.Cons l b r r2
  => IsSymbol l
  => Proxy l
  -> (a -> b)
  -> Builder (RowOptions r1) (RowOptions r2)
b'modify l f = Builder \r1 -> bunsafeModify (reflectSymbol l) f r1

b'delete
  :: forall l a r1 r2
   . IsSymbol l
  => Row.Lacks l r1
  => Row.Cons l a r1 r2
  => Proxy l
  -> Builder (RowOptions r2) (RowOptions r1)
b'delete l = Builder \r2 -> bunsafeDelete (reflectSymbol l) r2

b'rename
  :: forall l1 l2 a r1 r2 r3
   . IsSymbol l1
  => IsSymbol l2
  => Row.Cons l1 a r2 r1
  => Row.Lacks l1 r2
  => Row.Cons l2 a r2 r3
  => Row.Lacks l2 r2
  => Proxy l1
  -> Proxy l2
  -> Builder (RowOptions r1) (RowOptions r3)
b'rename l1 l2 = Builder \r1 -> bunsafeRename (reflectSymbol l1) (reflectSymbol l2) r1

b'merge
  :: forall r1 r2 r3 r4
   . Row.Union r1 r2 r3
  => Row.Nub r3 r4
  => RowOptions r1
  -> Builder (RowOptions r2) (RowOptions r4)
b'merge r1 = Builder \r2 -> unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce r1) (unsafeCoerce r2))

b'union
  :: forall r1 r2 r3
   . Row.Union r1 r2 r3
  => RowOptions r1
  -> Builder (RowOptions r2) (RowOptions r3)
b'union r1 = Builder \r2 -> unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce r1) (unsafeCoerce r2))

b'disjointUnion
  :: forall r1 r2 r3
   . Row.Union r1 r2 r3
  => Row.Nub r3 r3
  => RowOptions r1
  -> Builder (RowOptions r2) (RowOptions r3)
b'disjointUnion r1 = Builder \r2 -> unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce r1) (unsafeCoerce r2))

b'nub
  :: forall r1 r2
   . Row.Nub r1 r2
  => Builder (RowOptions r1) (RowOptions r2)
b'nub = Builder unsafeCoerce

------------------ hmap
instance hmapRowOptions ::
  ( RL.RowToList rin rl
  , MapRowOptionsWithIndex rl (ConstMapping fn) rin rout
  ) =>
  HMap fn (RowOptions rin) (RowOptions rout)
  where
  hmap =
    b'build
      <<< mapRowOptionsWithIndexBuilder (RLProxy :: RLProxy rl)
      <<< ConstMapping

instance hmapWithIndexRowOptions ::
  ( RL.RowToList rin rl
  , MapRowOptionsWithIndex rl fn rin rout
  ) =>
  HMapWithIndex fn (RowOptions rin) (RowOptions rout)
  where
  hmapWithIndex =
    b'build
      <<< mapRowOptionsWithIndexBuilder (RLProxy :: RLProxy rl)

class MapRowOptionsWithIndex (xs :: RL.RowList Type) f (as :: Row Type) (bs :: Row Type) | xs f -> bs, xs -> as where
  mapRowOptionsWithIndexBuilder :: RLProxy xs -> f -> Builder (RowOptions as) (RowOptions bs)

instance mapRowOptionsWithIndexCons ::
  ( IsSymbol sym
  , MappingWithIndex f (Proxy sym) a b
  , MapRowOptionsWithIndex rest f as bs'
  , Row.Cons sym a bx bs'
  , Row.Cons sym b bx bs
  ) =>
  MapRowOptionsWithIndex (RL.Cons sym a rest) f as bs
  where
  mapRowOptionsWithIndexBuilder _ f =
    b'modify prop (mappingWithIndex f prop)
      <<< mapRowOptionsWithIndexBuilder (RLProxy :: RLProxy rest) f
    where
    prop = Proxy :: Proxy sym

instance mapRowOptionsWithIndexNil :: MapRowOptionsWithIndex RL.Nil fn as as where
  mapRowOptionsWithIndexBuilder _ _ = identity

--- megamap

foreign import unsafeMegamap :: forall r1 r2 r3. RowOptions r1 -> { | r2 } -> RowOptions r3

class MegamapRowOptions (r1 :: Row Type) (r2 :: RowList Type) (r3 :: Row Type) | r2 -> r1 r3

instance megamapRowOptionsNil :: MegamapRowOptions () RL.Nil ()
instance megamapRowOptionsCons :: (Row.Cons sym a r1' r1, Row.Cons sym b r3' r3, MegamapRowOptions r1' rest2 r3') => MegamapRowOptions r1 (RL.Cons sym (a -> b) rest2) r3

megamap :: forall r1 r2 rl2 r3. RL.RowToList r2 rl2 => MegamapRowOptions r1 rl2 r3 => RowOptions r1 -> { | r2 } -> RowOptions r3
megamap = unsafeMegamap

-- util
asOptions :: forall r. { | r } -> RowOptions r
asOptions = unsafeCoerce

foreign import unsafeKeys :: forall r. RowOptions r -> Array String

class RowListKeys (r :: RowList Type) where
  rowListKeys :: forall proxy. proxy r -> Array String

instance rowListKeysNil :: RowListKeys RL.Nil where
  rowListKeys _ = []

instance rowListKeysCons :: (IsSymbol sym, RowListKeys rest) => RowListKeys (RL.Cons sym a rest) where
  rowListKeys _ = [ reflectSymbol (Proxy :: _ sym) ] <> rowListKeys (Proxy :: _ rest)

feelingLucky :: forall r1 rl1 r2 r3. Union r1 r2 r3 => RL.RowToList r1 rl1 => RowListKeys rl1 => RowOptions r3 -> Maybe { | r1 }
feelingLucky ro = if fromFoldable (rowListKeys (Proxy :: _ rl1)) == fromFoldable (unsafeKeys ro) then Just (unsafeCoerce ro) else Nothing