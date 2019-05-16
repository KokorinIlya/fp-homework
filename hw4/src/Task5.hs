{-# LANGUAGE ScopedTypeVariables #-}

module Task5
  ( AllocateT
  , allocate
  , release
  , runAllocateT
  , tryAll
  , resourceFork
  ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception (SomeException)
import Control.Monad (forM, forM_)
import Control.Monad.Catch (MonadCatch, MonadMask, catchAll, finally, mask_, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), ask, runReaderT)
import Control.Monad.Trans (lift)
import Data.Either (lefts)
import Data.Functor ((<$))
import Data.Hashable (Hashable, hashWithSalt)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
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
  { resources        :: M.Map ResourceKey Resource -- Shared between all threads
  , maxResouceNumber :: TVar Integer -- Shared between all threads
  , ownedResources   :: IORef [ResourceKey] -- Thread-local
  , mvarsToTake      :: IORef [MVar ()] -- Thread-local
  }

type AllocateT m a = ReaderT ResourceHolder m a

allocate :: (MonadIO m, MonadMask m) => IO a -> (a -> IO ()) -> AllocateT m (a, ResourceKey)
allocate resourceAquireAction resourceReleaseAction =
  mask_ $ do
    resource <- lift $ liftIO resourceAquireAction
    let releaseAction = resourceReleaseAction resource
    addResourceToMap resource releaseAction `catchAll` cleanResource releaseAction
  where
    addResourceToMap :: (MonadIO m, MonadMask m) => a -> IO () -> AllocateT m (a, ResourceKey)
    addResourceToMap resource releaseAction = do
      ResourceHolder {resources = curResourcesMap, maxResouceNumber = maxNumberRef, ownedResources = resListRef} <- ask
      curResourceKey <-
        lift $
        liftIO $
        atomically $ do
          curMaxNumber <- readTVar maxNumberRef
          let addedResourceKey = ResourceKey curMaxNumber
          writeTVar maxNumberRef (curMaxNumber + 1)
          M.insert (Resource 1 releaseAction) addedResourceKey curResourcesMap
          return addedResourceKey
      lift $
        liftIO $
        modifyIORef' resListRef (curResourceKey :) `catchAll`
        (\e -> atomically (M.delete curResourceKey curResourcesMap) >> throwM e)
      return (resource, curResourceKey)
    cleanResource :: (MonadIO m, MonadMask m) => IO () -> SomeException -> AllocateT m (a, ResourceKey)
    cleanResource releaseAction e = lift (liftIO releaseAction) `catchAll` (\_ -> return ()) >> throwM e

runAllocateT :: (MonadIO m, MonadMask m) => AllocateT m a -> m a
runAllocateT actionToRun = do
  (startMap, startResourceNumber) <- liftIO $ atomically $ (,) <$> M.new <*> newTVar 0
  aquiredResources <- liftIO $ newIORef []
  sonRefs <- liftIO $ newIORef []
  let mainResourceholder = ResourceHolder startMap startResourceNumber aquiredResources sonRefs
  runReaderT (actionToRun `finally` onFinishAction) mainResourceholder
  where
    onFinishAction :: (MonadIO m, MonadMask m) => AllocateT m ()
    onFinishAction = do
      resHolder@ResourceHolder {mvarsToTake = childsListRef} <- ask
      childsList <- lift $ liftIO $ readIORef childsListRef
      lift $ liftIO $ forM_ childsList $ \curChild -> takeMVar curChild `catchAll` (\_ -> return ())
      releaseResourcesFromMap resHolder

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
resourceFork forkFunction action =
  mask_ $ do
    sonEnvironment <- getNewEnvironment
    spawnChild sonEnvironment forkFunction action `catchAll` (\e -> cleanEnvironment >> throwM e)
  where
    getNewEnvironment :: (MonadIO m, MonadMask m) => AllocateT m ResourceHolder
    getNewEnvironment = do
      curHolder@ResourceHolder {resources = sharedResources, ownedResources = localOwnedResources} <- ask
      localResourceList <- lift $ liftIO $ readIORef localOwnedResources
      sonResourceListRef <- lift $ liftIO $ newIORef localResourceList
      sonMVarsToTake <- lift $ liftIO $ newIORef []
      lift $
        liftIO $
        atomically $
        forM_ localResourceList $ \localResourceKey -> do
          maybeResource <- M.lookup localResourceKey sharedResources
          case maybeResource of
            Nothing -> return ()
            Just curResource@Resource {holdersCount = curHoldersCount} -> do
              M.delete localResourceKey sharedResources
              M.insert curResource {holdersCount = curHoldersCount + 1} localResourceKey sharedResources
      return curHolder {ownedResources = sonResourceListRef, mvarsToTake = sonMVarsToTake}
    spawnChild :: (MonadIO m, MonadMask m) => ResourceHolder -> (m () -> m ()) -> AllocateT m () -> AllocateT m ()
    spawnChild childResHolder forkPerformer actionToDo = do
      ResourceHolder {mvarsToTake = childListRef} <- ask
      newChildRef <- lift $ liftIO newEmptyMVar
      lift $ liftIO $ modifyIORef' childListRef (newChildRef :)
      lift $ forkPerformer $ runReaderT (actionToDo `finally` onFinishChildAction newChildRef) childResHolder
    onFinishChildAction :: (MonadIO m, MonadMask m) => MVar () -> AllocateT m ()
    onFinishChildAction mvarToPut = do
      childResHolder@ResourceHolder {mvarsToTake = childsListRef} <- ask
      childsList <- lift $ liftIO $ readIORef childsListRef
      lift $ liftIO $ forM_ childsList $ \curChild -> takeMVar curChild `catchAll` (\_ -> return ())
      releaseResourcesFromMap childResHolder
      lift $ liftIO $ putMVar mvarToPut ()
    cleanEnvironment :: (MonadIO m, MonadMask m) => AllocateT m ()
    cleanEnvironment = do
      ResourceHolder {resources = sharedResources, ownedResources = localOwnedResources} <- ask
      localResourceList <- lift $ liftIO $ readIORef localOwnedResources
      lift $
        liftIO $
        atomically $
        forM_ localResourceList $ \localResourceKey -> do
          maybeResource <- M.lookup localResourceKey sharedResources
          case maybeResource of
            Nothing -> return ()
            Just curResource@Resource {holdersCount = curHoldersCount} -> do
              M.delete localResourceKey sharedResources
              M.insert curResource {holdersCount = curHoldersCount - 1} localResourceKey sharedResources

releaseResourcesFromMap :: (MonadIO m, MonadMask m) => ResourceHolder -> m ()
releaseResourcesFromMap ResourceHolder {resources = resourcesMap, ownedResources = threadResKeysRef} = do
  threadResKeys <- liftIO $ readIORef threadResKeysRef
  releaseActionsToPerform <- liftIO $ getActionsToCleanup threadResKeys resourcesMap
  liftIO $ doCleanupAll releaseActionsToPerform
  where
    getActionsToCleanup :: [ResourceKey] -> M.Map ResourceKey Resource -> IO [IO ()]
    getActionsToCleanup threadResKeys curMap = mask_ $ atomically $ updateMap curMap threadResKeys
    updateMap :: M.Map ResourceKey Resource -> [ResourceKey] -> STM [IO ()]
    updateMap _ [] = return []
    updateMap curMap (curKey:otherKeys) = do
      maybeCurRes <- M.lookup curKey curMap
      case maybeCurRes of
        Nothing -> updateMap curMap otherKeys
        Just Resource {holdersCount = 1, freeAction = curFreeAction} -> do
          M.delete curKey curMap
          otherFreeActions <- updateMap curMap otherKeys
          return $ curFreeAction : otherFreeActions
        Just curRes@Resource {holdersCount = n} -> do
          M.delete curKey curMap
          M.insert curRes {holdersCount = n - 1} curKey curMap
          updateMap curMap otherKeys
    doCleanupAll :: [IO ()] -> IO ()
    doCleanupAll actionsToPerform = do
      results <- forM actionsToPerform tryAll
      case lefts results of
        []   -> return ()
        ex:_ -> throwM ex

tryAll :: (MonadCatch m) => m a -> m (Either SomeException a)
tryAll action =
  (do result <- action
      return $ Right result) `catchAll`
  (return . Left)
