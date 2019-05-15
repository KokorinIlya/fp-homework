{-# LANGUAGE ScopedTypeVariables #-}

module Task5
  ( AllocateT
  , allocate
  , release
  , runAllocateT
  , tryAll
  , resourceFork
  ) where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception (SomeException)
import Control.Monad (forM, forM_, liftM)
import Control.Monad.Catch (MonadCatch, MonadMask, bracket, catchAll, mask_, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), ask, runReaderT)
import Control.Monad.Trans (lift)
import Data.Either (lefts)
import Data.Functor ((<$))
import Data.Hashable (Hashable, hashWithSalt)
import qualified ListT as LT
import qualified StmContainers.Map as M

data Resource = Resource
  { holdersCount :: Int
  , freeAction   :: IO ()
  }

newtype ResourceKey = ResourceKey
  { key :: Integer
  } deriving (Eq)

instance Hashable ResourceKey where
  hashWithSalt salt ResourceKey {key = curKey} = hashWithSalt salt curKey

data ResourceHolder = ResourceHolder
  { resources        :: M.Map ResourceKey Resource
  , maxResouceNumber :: TVar Integer
  }

type AllocateT m a = ReaderT ResourceHolder m a

tryAll :: (MonadCatch m) => m a -> m (Either SomeException a)
tryAll a = (Right `liftM` a) `catchAll` (return . Left)

allocate :: (MonadIO m, MonadMask m) => IO a -> (a -> IO ()) -> AllocateT m (a, ResourceKey)
allocate resourceAquireAction resourceReleaseAction =
  mask_ $ do
    resource <- lift $ liftIO resourceAquireAction
    let releaseAction = resourceReleaseAction resource
    addResourceToMap resource releaseAction `catchAll` cleanResource releaseAction
  where
    addResourceToMap :: (MonadIO m, MonadMask m) => a -> IO () -> AllocateT m (a, ResourceKey)
    addResourceToMap resource releaseAction = do
      ResourceHolder {resources = curResourcesMap, maxResouceNumber = maxNumberRef} <- ask
      curResourceKey <-
        lift $
        liftIO $
        atomically $ do
          curMaxNumber <- readTVar maxNumberRef
          let addedResourceKey = ResourceKey curMaxNumber
          writeTVar maxNumberRef (curMaxNumber + 1)
          M.insert (Resource 1 releaseAction) addedResourceKey curResourcesMap
          return addedResourceKey
      return (resource, curResourceKey)
    cleanResource :: (MonadIO m, MonadMask m) => IO () -> SomeException -> AllocateT m (a, ResourceKey)
    cleanResource releaseAction e = lift (liftIO releaseAction) `catchAll` (\_ -> return ()) >> throwM e

cleanResources :: (MonadIO m, MonadMask m) => ResourceHolder -> m ()
cleanResources ResourceHolder {resources = resourcesMap} = do
  releaseActionsToPerform <- liftIO $ getActionsToCleanup resourcesMap
  liftIO $ doCleanupAll releaseActionsToPerform
  where
    doCleanupAll :: [IO ()] -> IO ()
    doCleanupAll actionsToPerform = do
      results <- forM actionsToPerform tryAll
      case lefts results of
        []   -> return ()
        ex:_ -> throwM ex
    getActionsToCleanup :: M.Map ResourceKey Resource -> IO [IO ()]
    getActionsToCleanup curMap =
      mask_ $
      atomically $ do
        resourcesList <- LT.toList $ M.listT curMap
        let (listToRelease, listToDecrease) = divideResourceList resourcesList ([], [])
        forM_ listToRelease $ \(curKeyToRelease, _) -> M.delete curKeyToRelease curMap
        forM_ listToDecrease $ \(curKeyToDecrease, curResource@Resource {holdersCount = curHoldersCount}) -> do
          M.delete curKeyToDecrease curMap
          M.insert (curResource {holdersCount = curHoldersCount - 1}) curKeyToDecrease curMap
        return $ map getReleaseAction listToRelease
    divideResourceList ::
         [(ResourceKey, Resource)]
      -> ([(ResourceKey, Resource)], [(ResourceKey, Resource)])
      -> ([(ResourceKey, Resource)], [(ResourceKey, Resource)])
    divideResourceList [] lists = lists
    divideResourceList (curPair@(_, curResource):otherPairs) (listToClean, listToDecrease)
      | holdersCount curResource == 1 = divideResourceList otherPairs (curPair : listToClean, listToDecrease)
      | otherwise = divideResourceList otherPairs (listToClean, curPair : listToDecrease)
    getReleaseAction :: (ResourceKey, Resource) -> IO ()
    getReleaseAction (_, resource) = freeAction resource

runAllocateT :: (MonadIO m, MonadMask m) => AllocateT m a -> m a
runAllocateT actionToRun = bracket makeEnvironment cleanResources (runReaderT actionToRun)
  where
    makeEnvironment :: MonadIO m => m ResourceHolder
    makeEnvironment = do
      startMap <- liftIO $ atomically M.new
      startResourceNumber <- liftIO $ atomically $ newTVar 0
      return ResourceHolder {resources = startMap, maxResouceNumber = startResourceNumber}

release ::
     forall m. (MonadIO m, MonadMask m)
  => ResourceKey
  -> AllocateT m ()
release resourceKey =
  mask_ $ do
    ResourceHolder {resources = curResourcesMap} <- ask
    lift $ liftIO $ performFreeAction curResourcesMap
  where
    performFreeAction :: M.Map ResourceKey Resource -> IO ()
    performFreeAction curResourcesMap = do
      maybeFreeAction <- atomically $ getFreeAction curResourcesMap
      case maybeFreeAction of
        Nothing              -> return ()
        Just actionToPerform -> actionToPerform `catchAll` (\_ -> return ())
    getFreeAction :: M.Map ResourceKey Resource -> STM (Maybe (IO ()))
    getFreeAction resourceMap = do
      maybeResource <- M.lookup resourceKey resourceMap
      case maybeResource of
        Nothing -> return Nothing
        Just Resource {freeAction = curFreeAction} -> Just curFreeAction <$ M.delete resourceKey resourceMap

resourceFork :: (MonadIO m, MonadMask m) => (m () -> m ()) -> AllocateT m () -> AllocateT m ()
resourceFork forkFunction action = bracket updateEnvironment cleanResources (performFork forkFunction action)
  where
    updateEnvironment :: (MonadIO m, MonadMask m) => AllocateT m ResourceHolder
    updateEnvironment = do
      curHolder@ResourceHolder {resources = curResourcesMap} <- ask
      lift $
        liftIO $
        atomically $ do
          resourcesList <- LT.toList $ M.listT curResourcesMap
          forM_ resourcesList $ \(curKey, curResource@Resource {holdersCount = curHoldersCount}) -> do
            M.delete curKey curResourcesMap
            M.insert curResource {holdersCount = curHoldersCount + 1} curKey curResourcesMap
      return curHolder
    performFork :: (MonadIO m, MonadMask m) => (m () -> m ()) -> AllocateT m () -> ResourceHolder -> AllocateT m ()
    performFork forkImplementation actionToPerform curMap =
      ReaderT $ \_ -> forkImplementation (runReaderT actionToPerform curMap)
