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
  , class EqualFields
  , equalFields
  , class OrdFields
  , ordFields
  , class ShowFields
  , showFields
  --
  , unsafeOptionsGet
  ) where

import Prelude

import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Lacks, class Cons, class Nub, class Union)
import Prim.Row as R
import Prim.RowList (class RowToList, RowList, Cons, Nil)
import Record.Unsafe (unsafeDelete, unsafeSet)
import Record.Unsafe.Union (unsafeUnionFn)
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
  show a = showFields (Proxy :: Proxy rs) a

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
  showFields _ a =  (case v' of
    Nothing -> ""
    Just x -> (reflectSymbol (Proxy :: _ name) <> ": " <> show x)) <> showFields (Proxy :: Proxy tail) a
    where
    v' = get (Proxy :: Proxy name) a

instance showFieldsNil :: ShowFields Nil row where
  showFields _ _ = ""