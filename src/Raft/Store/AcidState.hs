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


import qualified Raft.Protocol as CS
import qualified Raft.Types as CS


type AcidStateStoreM st r = StateT (AcidStateStore st) IO r

newtype AcidStateStore st = AcidStateStore { stateVar :: AcidState [st]}

data AcidStateList st = AcidStateList {_latestCommit :: CS.Index
                                      , _store :: [(CS.Index,CS.Term,st)]
                                      }



queryST :: IsAcidic st => CS.Index -> Query (AcidStateList st) (Maybe (st,CS.Term))
queryST ix = do
    (AcidStateList _l as) <- ask
    return $ case dropWhile (\(i,t,a) -> i > ix) as of
        ((i,t,a):_) | i == ix   -> Just (a,t)
        _ -> Nothing


appendST :: IsAcidic st => CS.Index -> CS.Term -> st -> Update (AcidStateList st) ()
appendST i t a = do
    (AcidStateList l as) <- get
    put $ AcidStateList l ((i,t,a):as)

commitST :: IsAcidic st => CS.Index -> Update (AcidStateList st) ()
commitST ix = do
    (AcidStateList l as) <- get
    put $ AcidStateList ix as

truncateST :: IsAcidic st => CS.Index ->  Update (AcidStateList st) ()
truncateST ix = do
    (AcidStateList l as) <- get
    put $ AcidStateList l (takeWhile (\(i,t,a) -> i > ix) as)

-- $(makeAcidic (''AcidStateList st) ['queryST, 'appendST, 'commitST, 'truncateST])

data QueryST = QueryST CS.Index deriving Typeable
data AppendST st = AppendST CS.Index CS.Term st deriving Typeable
data CommitST = CommitST CS.Index deriving Typeable
data TruncateST = TruncateST CS.Index deriving Typeable



instance SafeCopy st => SafeCopy (AcidStateList st) where
    putCopy (AcidStateList (CS.Term ix) state) = contain $ safePut ix >> safePut state
    getCopy = contain $ liftM2 AcidStateList safeGet safeGet



-- runAcidStateStore :: (Fold.Foldable t)
--                => Free (CS.LogStoreF t st) r -> AcidStateStoreM st r
-- runAcidStateStore (Pure r) = return r
-- runAcidStateStore (Free x) = case x of
--     CS.LogQuery ix cont -> do _logQueryCase
--         AcidStateStore st <- get
--         res <- liftIO $ Sqlite.query conn "select value, term from store where ix = (?)" [ix]
--         -- runAcidStateStore $ cont (second CS.Term <$> listToMaybe res)
--     CS.LogStore ix (CS.Term term) xs next -> do _logStoreCase
--         -- AcidStateStore conn <- get
--         -- liftIO $ mapM_ (\(ixx, x) ->
--         --                    Sqlite.execute conn "insert into store (ix,value,term) values (?,?,?)" [ixx, x, term])
--         --                (zip [ix..] (Fold.toList xs))
--         -- runAcidStateStore next
--     CS.LogCommit ix next -> do _logCommitCase
--         -- AcidStateStore conn <- get
--         -- liftIO $ Sqlite.execute conn "insert into meta (latestCommit) values (?)" [ix]
--         -- runAcidStateStore next
--     CS.LogTruncate ix next -> do _logTruncateCase
--         -- AcidStateStore conn <- get
--         -- liftIO $ do
--         --     res <- Sqlite.query_ conn "select from meta (latestCommit)"
--         --     let [c] = fromMaybe [0] (listToMaybe res)
--         --     Sqlite.execute conn "delete from store where ix > (?)" [min ix c]
--         -- runAcidStateStore next

-- instance IsAcidic st => CS.Store (AcidStateStore st) where
--     type Value (AcidStateStore st) = st

-- instance IsAcidic st => CS.StoreIO (AcidStateStore st) where
--     interpret cmds s = do
--         (r, s') <- runStateT (runAcidStateStore cmds) s
--         return (s', r)