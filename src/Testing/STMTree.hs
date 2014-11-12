module Testing.STMTree where

import Testing.STMTree.Internal

import qualified Data.Tree as T
import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import qualified Data.Traversable as TR
import Control.Applicative

-- type STMTree a = T.Tree (TMVar a)


data STMTree a = STMTree {
  stmTree :: T.Tree (TMVar a)
, stmLoad :: TMVar ()  
}


newSTMTree :: T.Tree (TMVar a) -> IO (STMTree a)
newSTMTree t = do
  load <- newEmptyTMVarIO
  return $ STMTree t load

unloadSTMTree :: STMTree a -> STM (T.Tree a)
unloadSTMTree (STMTree tree load) = takeTMVar load 
                                    >> (TR.sequence $ readTMVar <$> tree)



modifyAllNodes :: a -> STMTree a -> IO ()
modifyAllNodes a (STMTree tree load) = atomically $ do
  tree' <- TR.sequence $ (flip swapTMVar a) <$> tree
  putTMVar load ()


loadSTMTree :: STMTree a -> STM Bool
loadSTMTree (STMTree _ l) = tryPutTMVar l ()