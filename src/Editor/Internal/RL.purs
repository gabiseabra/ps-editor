module Editor.Internal.RL
  ( (#:)
  , (#<>)
  , (<>#)
  , RL
  , class FoldlRL
  , stepFoldlRL
  , class FoldrRL
  , stepFoldrRL
  , class RowListGet
  , get
  , class RunFoldlRL
  , runFoldlRL
  , class RunFoldrRL
  , runFoldrRL
  , delete
  , empty
  , foldlRL
  , foldrRL
  , insert
  , merge
  , mergeFlipped
  , record
  , singleton
  , under
  )
  where

import Prelude

import Data.Symbol (class IsSymbol)
import Foreign (Foreign)
import Foreign as Foreign
import Prim.Row as Row
import Record as Rec
import Type.Proxy (Proxy(..))
import Type.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

data RL (rl :: RL.RowList Type)
  = RL Foreign

record :: forall rl r. RL.ListToRow rl r => RL rl -> Record r
record (RL rec) = Foreign.unsafeFromForeign rec

under :: forall r r' rl rl'
  .  RL.ListToRow rl r
  => RL.ListToRow rl' r'
  => (Record r -> Record r')
  -> Proxy rl'
  -> RL rl
  -> RL rl'
under f _ = RL <<< Foreign.unsafeToForeign <<< f <<< record

empty :: RL RL.Nil
empty = RL (Foreign.unsafeToForeign {})

singleton :: forall proxy k a r
  .  IsSymbol k
  => Row.Cons k a () r
  => proxy k
  -> a
  -> RL (RL.Cons k a RL.Nil)
singleton _ a = RL (Foreign.unsafeToForeign rec)
  where k = Proxy :: Proxy k
        rec = Rec.insert k a {} :: Record r

infixl 5 singleton as #:

class RowListGet (rl :: RL.RowList Type) k a | rl k -> a where
  get :: forall proxy. IsSymbol k => proxy k -> RL rl -> a
instance rowListGetFound ::
  ( IsSymbol k
  , RL.ListToRow (RL.Cons k a rl') r
  , Row.Cons k a r' r
  ) => RowListGet (RL.Cons k a rl') k a where
  get _ = Rec.get (Proxy :: Proxy k) <<< record
else instance rowListGetPass ::
  ( RowListGet rl' k' a
  ) => RowListGet (RL.Cons k x rl') k' a where
  get k rl = get k (unsafeCoerce rl :: RL rl')

insert :: forall proxy r r' rl k a
  .  IsSymbol k
  => RL.ListToRow rl r -- rl -> r
  => RL.RowListNub (RL.Cons k a rl) (RL.Cons k a rl) -- verify that key is unique
  => Row.Lacks k r
  => Row.Cons k a r r'
  => proxy k
  -> a
  -> RL rl
  -> RL (RL.Cons k a rl)
insert _ a = under (Rec.insert (Proxy :: Proxy k) a) (Proxy :: Proxy (RL.Cons k a rl))

delete :: forall proxy r r' rl rl' k a
  .  IsSymbol k
  => RL.ListToRow rl r
  => RL.ListToRow rl' r'
  => RL.RowListRemove k rl rl'
  => Row.Cons k a r' r 
  => Row.Lacks k r'
  => proxy k
  -> RL rl
  -> RL rl'
delete _ = under (Rec.delete (Proxy :: Proxy k)) (Proxy :: Proxy rl')

merge :: forall rl rl' rl'' r r' r''
  .  RL.RowListAppend rl rl' rl''
  => RL.RowListNub rl'' rl'' -- union has no duplicate keys
  => RL.ListToRow rl r
  => RL.ListToRow rl' r'
  => RL.ListToRow rl'' r''
  => Row.Union r r' r''
  => Row.Nub r'' r''
  => RL rl
  -> RL rl'
  -> RL rl''
merge (RL f) (RL f') = RL $ Foreign.unsafeToForeign r''
  where r = Foreign.unsafeFromForeign f :: Record r
        r' = Foreign.unsafeFromForeign f' :: Record r'
        r'' = Rec.merge r r' :: Record r''

mergeFlipped = flip merge

infixl 4 merge as <>#
infixl 4 mergeFlipped as #<>

class IsSymbol k <= FoldrRL f k a b | f k -> a b where
  stepFoldrRL :: f -> Proxy k -> a -> b -> b

class RunFoldrRL :: Type -> Type -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class RunFoldrRL f a b rl focus | focus -> a, f -> b where
  runFoldrRL :: Proxy focus -> f -> b -> RL rl -> b
instance _runFoldrRLCons ::
  ( IsSymbol k 
  , RowListGet rl k a
  , RunFoldrRL f a' b rl tail
  , FoldrRL f k a b
  ) => RunFoldrRL f a b rl (RL.Cons k a tail) where
  runFoldrRL _ f b0 rl = stepFoldrRL f k a $ runFoldrRL (Proxy :: Proxy tail) f b0 rl
    where k = (Proxy :: Proxy k)
          a = get k rl
else instance _runFoldrRLNil :: RunFoldrRL f Void b rl RL.Nil where
  runFoldrRL _ _ b0 _ = b0

foldrRL :: forall f a b rl
  .  RunFoldrRL f a b rl rl
  => f
  -> b
  -> RL rl
  -> b
foldrRL = runFoldrRL (Proxy :: Proxy rl)

class IsSymbol k <= FoldlRL f k a b | f k -> a b where
  stepFoldlRL :: f -> Proxy k -> b -> a -> b

class RunFoldlRL :: Type -> Type -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class RunFoldlRL f a b rl focus | focus -> a, f -> b where
  runFoldlRL :: Proxy focus -> f -> b -> RL rl -> b
instance _runFoldlRLCons ::
  ( IsSymbol k 
  , RowListGet rl k a
  , RunFoldlRL f a' b rl tail
  , FoldlRL f k a b
  ) => RunFoldlRL f a b rl (RL.Cons k a tail) where
  runFoldlRL _ f b0 rl = runFoldlRL (Proxy :: Proxy tail) f (stepFoldlRL f k b0 a) rl
    where k = (Proxy :: Proxy k)
          a = get k rl
else instance _runFoldlRLNil :: RunFoldlRL f Void b rl RL.Nil where
  runFoldlRL _ _ b0 _ = b0

foldlRL :: forall f a b rl
  .  RunFoldlRL f a b rl rl
  => f
  -> b
  -> RL rl
  -> b
foldlRL = runFoldlRL (Proxy :: Proxy rl)
