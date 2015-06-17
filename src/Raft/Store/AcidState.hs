{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Raft.Store.AcidState where

import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Reader

import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy

import Data.Typeable

import qualified Data.Foldable as Fold

import qualified Raft.Types as CS


type AcidStateStoreM st r = 
    StateT (AcidStateStore st) IO r

newtype AcidStateStore st = 
    AcidStateStore { stateVar :: AcidState (AcidStateList st)}

data AcidStateList st = 
    AcidStateList {_latestCommit :: CS.Index
                  , _store :: [(CS.Index,CS.Term,st)]
                  }



queryST :: IsAcidic st => CS.Index -> Query (AcidStateList st) (Maybe (st,CS.Term))
queryST ix = do
    (AcidStateList _l as) <- ask
    return $ case dropWhile (\(i,_t,_a) -> i > ix) as of
        ((i,t,a):_) | i == ix   -> Just (a,t)
        _ -> Nothing


appendST :: (IsAcidic st, Traversable t)
         => CS.Index -> CS.Term -> t st -> Update (AcidStateList st) ()
appendST ix term sts = do
    (AcidStateList l as) <- get
    let tups = zipWith (\i a -> (i,term,a)) [ix..] (Fold.toList sts)
    put $ AcidStateList l (tups ++ as)

commitST :: IsAcidic st => CS.Index -> Update (AcidStateList st) ()
commitST ix = do
    (AcidStateList _l as) <- get
    put $ AcidStateList ix as

truncateST :: IsAcidic st => CS.Index ->  Update (AcidStateList st) ()
truncateST ix = do
    (AcidStateList l as) <- get
    put $ AcidStateList (min ix l) (takeWhile (\(i,_,_) -> i > ix) as)

-- $(makeAcidic (''AcidStateList st) ['queryST, 'appendST, 'commitST, 'truncateST])

data QueryST st = QueryST CS.Index deriving Typeable
data AppendST st = AppendST CS.Index CS.Term [st] deriving Typeable
data CommitST st = CommitST CS.Index deriving Typeable
data TruncateST st = TruncateST CS.Index deriving Typeable


instance SafeCopy CS.Term where
    putCopy (CS.Term ix) = contain (safePut ix)
    getCopy = contain $ CS.Term <$> safeGet
instance Method CS.Term where
    type MethodResult CS.Term = ()
    type MethodState CS.Term = Int


instance SafeCopy (QueryST st) where
    putCopy (QueryST ix) = contain (safePut ix)
    getCopy = contain $ QueryST <$> safeGet
instance (Typeable st, SafeCopy st) => Method (QueryST st) where
    type MethodResult (QueryST st) = (Maybe (st,CS.Term))
    type MethodState (QueryST st) = AcidStateList st
instance (Typeable st, SafeCopy st) => UpdateEvent (QueryST st)
instance (Typeable st, SafeCopy st) => QueryEvent (QueryST st)


instance SafeCopy st => SafeCopy (AppendST st) where
    putCopy (AppendST ix t st) = contain $ do
        safePut ix
        safePut t
        safePut st
    getCopy = contain $ AppendST <$> safeGet <*> safeGet <*> safeGet
instance (Typeable st, SafeCopy st) => Method (AppendST st) where
    type MethodResult (AppendST st) = ()
    type MethodState (AppendST st) = AcidStateList st
instance (Typeable st, SafeCopy st) => UpdateEvent (AppendST st)


instance SafeCopy (CommitST st) where
    putCopy (CommitST ix) = contain (safePut ix)
    getCopy = contain $ CommitST <$> safeGet
instance (Typeable st, SafeCopy st) => Method (CommitST st) where
    type MethodResult (CommitST st) = ()
    type MethodState (CommitST st) = AcidStateList st
instance (Typeable st, SafeCopy st) => UpdateEvent (CommitST st)


instance SafeCopy (TruncateST st) where
    putCopy (TruncateST ix) = contain (safePut ix)
    getCopy = contain $ TruncateST <$> safeGet
instance (Typeable st, SafeCopy st) => Method (TruncateST st) where
    type MethodResult (TruncateST st) = ()
    type MethodState (TruncateST st) = AcidStateList st
instance (Typeable st, SafeCopy st) => UpdateEvent (TruncateST st)


instance SafeCopy st => SafeCopy (AcidStateList st) where
    putCopy (AcidStateList ix state) = contain $ do
        safePut ix
        safePut (map (\(i,CS.Term t,a) -> (i,t,a)) state)
    getCopy = contain $ 
        AcidStateList 
        <$> safeGet
        <*> (map (\(i,t,a) -> (i,CS.Term t,a)) <$> safeGet)

instance (Typeable st, IsAcidic st) => IsAcidic (AcidStateList st) where
    acidEvents =
        [QueryEvent (\(QueryST ix) -> queryST ix)
        ,UpdateEvent (\(AppendST i t a) -> appendST i t a)
        ,UpdateEvent (\(CommitST ix) -> commitST ix)
        ,UpdateEvent (\(TruncateST ix) -> truncateST ix)]



runAcidStateStore :: (Fold.Foldable t, Typeable st, SafeCopy st)
               => Free (CS.LogStoreF t st) r -> AcidStateStoreM st r
runAcidStateStore (Pure r) = return r
runAcidStateStore (Free x) = case x of
    CS.LogQuery ix cont -> do
        AcidStateStore st <- get
        runAcidStateStore . cont =<< query' st (QueryST ix)

    CS.LogStore ix term xs next -> do
        AcidStateStore st <- get 
        update' st (AppendST ix term (Fold.toList xs))
        runAcidStateStore next
    
    CS.LogCommit ix next -> do
        AcidStateStore st <- get
        update' st (CommitST ix)
        runAcidStateStore next

    CS.LogTruncate ix next -> do
        AcidStateStore st <- get
        update' st (TruncateST ix)
        runAcidStateStore next


instance CS.Store (AcidStateStore st) where
    type Value (AcidStateStore st) = st

instance (Typeable st, SafeCopy st)
        => CS.StoreIO (AcidStateStore st) where
    interpret cmds s = do
        (r, s') <- runStateT (runAcidStateStore cmds) s
        return (s', r)