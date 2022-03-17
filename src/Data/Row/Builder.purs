module Data.Row.Builder
  ( Builder
  , build
  , buildFromScratch
  , flip
  , insert
  , modify
  , delete
  , rename
  , merge
  , union
  , disjointUnion
  , nub
  ) where

import Prelude hiding (flip)

import Data.Function (flip) as Function
import Data.Function.Uncurried (runFn2)
import Data.Row.Options (RowOptions, options)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Record.Unsafe.Union (unsafeUnionFn)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

foreign import copyRowOptions :: forall r1. RowOptions r1 -> RowOptions r1
foreign import unsafeInsert :: forall a r1 r2. String -> a -> RowOptions r1 -> RowOptions r2
foreign import unsafeModify :: forall a b r1 r2. String -> (a -> b) -> RowOptions r1 -> RowOptions r2
foreign import unsafeDelete :: forall r1 r2. String -> RowOptions r1 -> RowOptions r2
foreign import unsafeRename :: forall r1 r2. String -> String -> RowOptions r1 -> RowOptions r2

newtype Builder a b = Builder (a -> b)

build :: forall r1 r2. Builder (RowOptions r1) (RowOptions r2) -> RowOptions r1 -> RowOptions r2
build (Builder b) r1 = b (copyRowOptions r1)

buildFromScratch :: forall r. Builder (RowOptions ()) (RowOptions r) -> RowOptions r
buildFromScratch = Function.flip build (options {})

flip :: forall r1 r2 r3. (RowOptions r1 -> Builder (RowOptions r2) (RowOptions r3)) -> RowOptions r2 -> Builder (RowOptions r1) (RowOptions r3)
flip f b = Builder \a -> build (f a) b

derive newtype instance semigroupoidBuilder :: Semigroupoid Builder
derive newtype instance categoryBuilder :: Category Builder

insert
  :: forall l a r1 r2
   . Row.Cons l a r1 r2
  => Row.Lacks l r1
  => IsSymbol l
  => Proxy l
  -> a
  -> Builder (RowOptions r1) (RowOptions r2)
insert l a = Builder \r1 -> unsafeInsert (reflectSymbol l) a r1

modify
  :: forall l a b r r1 r2
   . Row.Cons l a r r1
  => Row.Cons l b r r2
  => IsSymbol l
  => Proxy l
  -> (a -> b)
  -> Builder (RowOptions r1) (RowOptions r2)
modify l f = Builder \r1 -> unsafeModify (reflectSymbol l) f r1

delete
  :: forall l a r1 r2
   . IsSymbol l
   => Row.Lacks l r1
   => Row.Cons l a r1 r2
   => Proxy l
   -> Builder (RowOptions r2) (RowOptions r1)
delete l = Builder \r2 -> unsafeDelete (reflectSymbol l) r2

rename :: forall l1 l2 a r1 r2 r3
   . IsSymbol l1
  => IsSymbol l2
  => Row.Cons l1 a r2 r1
  => Row.Lacks l1 r2
  => Row.Cons l2 a r2 r3
  => Row.Lacks l2 r2
  => Proxy l1
  -> Proxy l2
  -> Builder (RowOptions r1) (RowOptions r3)
rename l1 l2 = Builder \r1 -> unsafeRename (reflectSymbol l1) (reflectSymbol l2) r1


merge
  :: forall r1 r2 r3 r4
   . Row.Union r1 r2 r3
  => Row.Nub r3 r4
  => RowOptions r1
  -> Builder (RowOptions r2) (RowOptions r4)
merge r1 = Builder \r2 -> unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce r1) (unsafeCoerce r2))


union
  :: forall r1 r2 r3
   . Row.Union r1 r2 r3
  => RowOptions r1
  -> Builder (RowOptions r2) (RowOptions r3)
union r1 = Builder \r2 -> unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce r1) (unsafeCoerce r2))

disjointUnion
  :: forall r1 r2 r3
   . Row.Union r1 r2 r3
  => Row.Nub r3 r3
  => RowOptions r1
  -> Builder (RowOptions r2) (RowOptions r3)
disjointUnion r1 = Builder \r2 -> unsafeCoerce (runFn2 unsafeUnionFn (unsafeCoerce r1) (unsafeCoerce r2))

nub
  :: forall r1 r2
   . Row.Nub r1 r2
  => Builder (RowOptions r1) (RowOptions r2)
nub = Builder unsafeCoerce