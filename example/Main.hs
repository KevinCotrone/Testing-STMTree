module Main where

import Testing.STMTree
import Control.Concurrent.STM.TMVar
import Control.Concurrent.Async

import Data.Tree
import Control.Monad.STM
import Control.Concurrent


main = do
  ts@(l1,l2,l3,l4,l5,l6,l7) <- initialTMVars
  let tTree = tmTree ts-- Get the basic tmvar tree
  stmTree <- newSTMTree tTree
  --Fork a thread to watch the tree and print it on loaded change
  async $ printTreeOnLoad stmTree
  threadDelay (5 * 10^6)
  modifyAllNodes 100 stmTree
  threadDelay (5 * 10^6)
  modifyAllNodes 200 stmTree
  return ()


printTreeOnLoad stmTree = do
  unTree <- atomically $ waitForSTMTree stmTree
  print unTree
  printTreeOnLoad stmTree


initialTMVars :: IO (TMVar Int, TMVar Int, TMVar Int, TMVar Int, TMVar Int, TMVar Int, TMVar Int)
initialTMVars = do
  l1 <- newTMVarIO 10
  l2 <- newTMVarIO 20
  l3 <- newTMVarIO 30
  l4 <- newTMVarIO 40
  l5 <- newTMVarIO 50
  l6 <- newTMVarIO 60
  l7 <- newTMVarIO 70
  return (l1,l2,l3,l4,l5,l6,l7)



tmTree :: (TMVar Int, TMVar Int, TMVar Int, TMVar Int, TMVar Int, TMVar Int, TMVar Int) -> Tree (TMVar Int)
tmTree (l1,l2,l3,l4,l5,l6,l7) = Node l1 [Node l2 [n2,n3,n4]]
  where n2 = Node l3 [Node l4 []]
        n3 = Node l5 []
        n4 = Node l6 [Node l7 []]
