{-# LANGUAGE FlexibleContexts #-}
module DotGraph where

import Lambda
import Lambda.Term (dist)
import Control.Monad.Reader
import Control.Monad.State 
import Control.Monad ((>=>))
import Control.Applicative ((<$>),(<*>))
import Data.Maybe (fromJust)

--Additionally, we want to display the Term as a hierachical tree
--using DOT. It means we need to serialize a Term value as a graph
--specification written in the DOT language.

type NodeId  = Int
type PortId  = Int
data LineStyle = Dotted | Solid deriving (Eq, Show)
data Node a  = Node a NodeId deriving (Eq, Show)
data Edge    = Edge NodeId LineStyle NodeId deriving (Eq, Show)
data Graph a = Graph [Node a] [Edge] deriving (Eq, Show)
type M a = ReaderT [(String,NodeId)] (State (NodeId, Graph String)) a

--We use a state monad to generate unique IDs for each node in the
--Term, and with which we'll be able to label the edges.

newNodeId = get >>= \(i, g) -> put (i + 1, g) >> return i
putNode n = get >>= \(i, Graph ns es) -> put (i, Graph (n:ns) es)
putEdge e = get >>= \(i, Graph ns es) -> put (i, Graph ns (e:es))
newNode f = newNodeId >>= \i -> putNode (f i) >> return i
lookupBinder :: String -> M (Maybe NodeId)
lookupBinder v = lookup v <$> ask

toNode :: Expr -> M NodeId
toNode = fold aux
  where
    aux :: Term (M NodeId) -> M NodeId
    aux (Var (V v)) = do
      i <- newNode $ Node v
      lookupBinder v >>= maybe (return ()) (putEdge . Edge i Dotted) 
      return i
    aux (Lam (V v) t) = do
      i <- newNode $ Node $ "λ" ++ v
      local ((v, i):) t >>= putEdge . Edge i Solid
      return i
    aux (App f e) = do
      i <- newNode $ Node "@"
      f >>= putEdge . Edge i Solid
      e >>= putEdge . Edge i Solid
      return i

toGraph :: Expr -> Graph String
toGraph e = snd $ snd $ runState (runReaderT (toNode e) []) (0, Graph [] [])

toDot :: Expr -> String
toDot e = unlines $
  ["digraph g {"
  ,"ordering = out; pad = 1.0;"
  ,"node [shape = none, height = .06 ];" ] ++
  map showNode (reverse ns) ++
  map showEdge (reverse es) ++ ["}"]
  where
    Graph ns es = toGraph e
    showNode (Node s i) = "node" ++ show i ++ "[label=\"" ++ s ++ "\"]"
    showEdge (Edge i style j) = "\"node" ++ show i ++ 
      "\" -> \"node" ++ show j ++ "\" [ " ++ showStyle style ++ ", arrowsize = 0.5];"
    showStyle Dotted = "style = dotted, constraint = false"
    showStyle Solid = "style = solid"
