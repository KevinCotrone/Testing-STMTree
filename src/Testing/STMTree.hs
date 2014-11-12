module Testing.STMTree where

import Testing.STMTree.Internal

import qualified Data.Tree as T
import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import qualified Data.Traversable as TR
import Control.Applicative
import Control.Monad

-- type STMTree a = T.Tree (TMVar a)


data STMTree a = STMTree {
  stmTree :: T.Tree (TMVar a)
, stmLoad :: TMVar ()  
}


newSTMTree :: T.Tree (TMVar a) -> IO (STMTree a)
newSTMTree t = do
  load <- newEmptyTMVarIO
  return $ STMTree t load

waitForSTMTree :: STMTree a -> STM (T.Tree a)
waitForSTMTree (STMTree tree load) = takeTMVar load 
                                    >> (TR.sequence $ readTMVar <$> tree)



modifyAllNodes :: a -> STMTree a -> IO ()
modifyAllNodes a (STMTree tree load) = atomically $ do
  _ <- TR.sequence $ (flip swapTMVar a) <$> tree
  putTMVar load ()


notifySTMTree :: STMTree a -> STM Bool
notifySTMTree (STMTree _ l) = tryPutTMVar l ()

loopSTMTree :: STMTree a -> (T.Tree a -> IO ()) -> IO ()
loopSTMTree sTree f = forever $ do
  tree <- atomically $ waitForSTMTree sTree
  f tree
