module Task4 where

import Control.Concurrent.STM.TVar (TVar, modifyTVar, modifyTVar', newTVar, readTVar, writeTVar)
import Control.Monad.STM (atomically, catchSTM)

newtype Hash a =
  Hash Int

class Hashable a where
  hashCode :: a -> Hash a

data ConcurrentHashTable k v = ConcurrentHashTable
  { size :: TVar Int
  }
