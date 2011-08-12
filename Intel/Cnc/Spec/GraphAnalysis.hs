{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NamedFieldPuns #-}

module Intel.Cnc.Spec.GraphAnalysis 
     (BasicCycleAnalysis(..), 
      basicCycleAnalysis,
      tests_graphanalysis)
where

import StringTable.Atom
import qualified StringTable.AtomMap as AM
import Data.List as L
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.Util hiding (app)
import Prelude hiding ((&&), (==), (<=))
import qualified Prelude as P

import Test.HUnit
import qualified Data.Graph.Inductive as G

-- import Data.Graph.Analysis.Algorithms.Common (cyclesIn') -- from package Graphalyze

import Intel.Cnc.Spec.GatherGraph (exampleGraph)
import Text.PrettyPrint.HughesPJClass

----------------------------------------------------------------------------------------------------

-- <DUPLICATED CODE FROM GRAPHALYZE PACKAGE.>  (To avoid the extra dependencies.)
-- This was distributed with the following further-reduced BSD-style license:
--
      -- Copyright (c) 2008, Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>
      -- All rights reserved.

      -- Redistribution and use in source and binary forms, with or without
      -- modification, are permitted provided that the following conditions are met:

      -- 1. Redistributions of source code must retain the above copyright notice,
      --    this list of conditions and the following disclaimer.
      -- 2. Redistributions in binary form must reproduce the above copyright
      --    notice, this list of conditions and the following disclaimer in the
      --    documentation and/or other materials provided with the distribution.

      -- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
      -- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
      -- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
      -- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
      -- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
      -- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
      -- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
      -- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
      -- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
      -- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
      -- POSSIBILITY OF SUCH DAMAGE.

import Control.Arrow(first)
import Data.Function (on)
import Control.Monad (ap)

-- | A grouping of 'LNode's.
type LNGroup a = [G.LNode a]
-- | A grouping of 'Node's.
type NGroup = [G.Node]

addLabels    :: (G.Graph g) => g a b -> [G.Node] -> [G.LNode a]
addLabels gr = map (ap (,) (fromJust . G.lab gr))

-- | Return true if and only if the list contains a single element.
single     :: [a] -> Bool
single [_] = True
single  _  = False

-- | Makes the graph a simple one, by removing all duplicate edges and loops.
--   The edges removed if duplicates exist are arbitrary.
mkSimple :: (G.DynGraph gr) => gr a b -> gr a b
mkSimple = G.gmap simplify
    where
      rmLoops n = filter ((/=) n . snd)
      rmDups = nubBy ((P.==) `on` snd)
      simpleEdges n = rmDups . rmLoops n
      simplify (p,n,l,s) = (p',n,l,s')
          where
            p' = simpleEdges n p
            s' = simpleEdges n s

-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree             :: (G.DynGraph g) => G.Decomp g a b -> [NGroup]
pathTree (Nothing,_) = []
pathTree (Just ct,g)
    | G.isEmpty g = []
    | null sucs = [[n]]
    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
    where
      n = G.node' ct
      sucs = G.suc' ct
      -- Avoid infinite loops by not letting it continue any further
      ct' = makeLeaf ct
      g' = ct' G.& g
      subPathTree gr n' = pathTree $ G.match n' gr

-- | Remove all outgoing edges
makeLeaf           :: G.Context a b -> G.Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
    where
      -- Ensure there isn't an edge (n,n)
      p' = filter (\(_,n') -> n' /= n) p

-- | Find all cycles in the given graph.
cyclesIn   :: (G.DynGraph g) => g a b -> [LNGroup a]
cyclesIn g = map (addLabels g) (cyclesIn' g)

-- | Find all cycles in the given graph, returning just the nodes.
cyclesIn' :: (G.DynGraph g) => g a b -> [NGroup]
cyclesIn' = concat . unfoldr findCycles . mkSimple

-- | Find all cycles containing a chosen node.
findCycles :: (G.DynGraph g) => g a b -> Maybe ([NGroup], g a b)
findCycles g
    | G.isEmpty g = Nothing
    | otherwise = Just . getCycles . G.matchAny $ g
    where
      getCycles (ctx,g') = (cyclesFor (ctx, g'), g')

-- | Find all cycles for the given node.
cyclesFor :: (G.DynGraph g) => G.GDecomp g a b -> [NGroup]
cyclesFor = map init .
            filter isCycle .
            pathTree .
            first Just
    where
      isCycle p = not (single p) P.&& (head p P.== last p)

-- </DUPLICATE>



----------------------------------------------------------------------------------------------------
-- Type definitions and functions for graph analyses.
----------------------------------------------------------------------------------------------------

-- | This represents the result of a basic graph analysis to determine
-- cycles.  The nodes are re-indexed according to a scheme that
-- assigns the same number to nodes within a cycle (e.g. they are
-- "grouped").
data BasicCycleAnalysis = BasicCycleAnalysis 
  {
    -- Map node names onto their new "collapsed" indices.
    index_map      :: AM.AtomMap Int,
    -- The same in reverse:
    rev_index_map  :: M.Map Int (S.Set CncGraphNode),
    -- Map nodes onto their up/downstream taking into account the
    -- grouping (e.g. a dependency on a node implies dependencies on
    -- all other nodes sharing the same group.
    downstream_map :: AM.AtomMap (S.Set CncGraphNode),
    upstream_map   :: AM.AtomMap (S.Set CncGraphNode)
  }
 deriving Show

-- Would be nice to DERIVE this:
instance Pretty BasicCycleAnalysis where 
 pPrint BasicCycleAnalysis{index_map, rev_index_map, upstream_map, downstream_map} =
   text "BasicCycleAnalysis {" $$
   nest 4 (
	   text "index_map = "      <> pPrint index_map      <> text ", " $$
	   text "rev_index_map = "  <> pPrint rev_index_map  <> text ", " $$
	   text "upstream_map = "   <> pPrint upstream_map   <> text ", " $$
	   text "downstream_map = " <> pPrint downstream_map
	  ) $$
   text "}"


-- | Performs an analysis to produce a BasicCycleAnalysis
--   For efficiency we would expect that this analysis is performed
--   once and shared between multiple plugins.
--
-- NOTE: Currently only step collections are included in the counter map!
basicCycleAnalysis :: CncSpec -> BasicCycleAnalysis
basicCycleAnalysis (spec@CncSpec{graph, steps, items, reductions, nodemap, realmap}) = 
    BasicCycleAnalysis {index_map=counter_map, rev_index_map=rev_counter_map, upstream_map, downstream_map}
  where 
      indexToNamed nd = case G.lab graph nd of 
		        Nothing -> error$ "basicCycleAnalysis: Node not found in graph: "++ show nd
		        Just x -> x
      namedToIndex   = fst . (G.mkNode_ nodemap)

      -- NOTE: Only "active" collections which can put new instances into
      -- other parts of the graph (currently only step collections) are
      -- considered for the purposes of cycle calculation.

      -- Thus remove reduction/item collections before computing cycles (interested in control only):
      pruned_graph = G.delNodes (map (namedToIndex . CGItems) $ AM.keys items) $
		     G.delNodes (map (namedToIndex . CGReductions) $ AM.keys reductions) $
		     graph 
      -- TODO: ALTERNATIVELY: Rebuild the graph with only step collections.
      --step_only_graph = stepOnlyGraph graph

      -- NOTE: This includes special_environment_name:
--      all_step_nds = S.fromList$ map (namedToIndex . CGSteps) (AS.toList steps)

      cycsets = joinCycles$ cyclesIn' pruned_graph
		      
      -- Remove all nodes that are in cycles to find those that remain:
--      non_cycle_nodes = S.toList $ foldl' S.difference all_step_nds cycsets

      all_names :: S.Set G.Node
      all_names = S.fromList$ map namedToIndex $ M.keys realmap
      non_cycle_nodes = S.toList$ foldl' S.difference all_names cycsets

      -- Combine the nodes in and out of cycles:
      allnodesets :: [S.Set G.Node] = cycsets ++ (map S.singleton $ non_cycle_nodes)      

      -- Map every node onto exactly one counter.  Nodes in a cycle must have the same counter.
      counter_map :: AM.AtomMap Int  = fromSetList $ nodesets_counters
      nodesets_counters = zip (map (S.map (graphNodeName . indexToNamed)) allnodesets) [0..] 
      --num_counters = length allnodesets
      -- For convenience, we store a map in the other direction as well, from counter -> nodeset:
      rev_counter_map = M.fromList $ 
			zip [0..] (map (S.map (fromJust . G.lab graph)) allnodesets)

      -- Next we compute the upstream dependencies of entire cycles taken together:
      cycs_wname = L.map getSteps cycsets
      -- getSteps: convert a set of FGL Nodes to a set of CGSteps
      getSteps = S.fromList . filter isStepC . map (fromJust . G.lab graph) . S.toList 
      
      -- For each node we store its up/down-stream dependencies.  If the node is part of a
      -- cycles, its up/down-stream deps are those of the entire cycle:
      upstream_map :: AM.AtomMap (S.Set CncGraphNode)   = make_map upstreamNbrs   
      downstream_map :: AM.AtomMap (S.Set CncGraphNode) = make_map downstreamNbrs 
      
      make_map getnbrs  =
             let
                 cycnbrs = L.map (\ set -> S.difference (setNbrs set) set)   cycs_wname
                 -- setNbrs: take the combined upstreams of a set of nodes:
		 setNbrs = S.fromList .  concat . map (getnbrs spec) . S.toList 
	     in 
	        fromSetList $ 
		(map (dosingle getnbrs) non_cycle_nodes ++
	        zip (map (S.map (graphNodeName . indexToNamed)) cycsets) cycnbrs)

      dosingle getnbrs nd = 
	     let name = indexToNamed nd in 
	       (S.singleton (graphNodeName name), 
		S.fromList$ getnbrs spec name)


----------------------------------------------------------------------------------------------------
-- Helpers/Utilities:
----------------------------------------------------------------------------------------------------

--fromSetList :: P.Ord a => [(S.Set a, b)] -> M.Map a b
fromSetList ::[(S.Set Atom, b)] -> AM.AtomMap b
fromSetList = 
   foldl' (\ map (set,val) -> 
	   S.fold (\ nd mp -> AM.insert nd val mp)
	          map set)
          AM.empty


-- Join together nodes that participate in overlapping cycles:
-- FIXME!!! Inefficient quadratic algorithm:
joinCycles :: (P.Ord a) => [[a]] -> [S.Set a]
joinCycles cycs = foldl' foldin [] (map S.fromList cycs)
 where 
  foldin [] cyc      = [cyc]
  foldin (hd:tl) cyc = if S.null (S.intersection hd cyc)
		       then hd : foldin tl cyc 
		       else (S.union cyc hd) : tl


----------------------------------------------------------------------------------------------------  
-- Unit Tests:
----------------------------------------------------------------------------------------------------  

testg :: G.Gr () String
testg = G.mkGraph (zip [1..7] (repeat ())) 
    [(1,2,""), (2,3,""), (3,4,""), (4,5,""), (5,6,""), (6,7,""),
     -- Close some cycles.
     (4,2,""), (7,6,""), (7,3,"")
    ]
testc = cyclesIn' testg

tests_graphanalysis = 
    testSet "CodegenShared" 
      [ testCase "" "joinCycles connected1"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles testc
      , testCase "" "joinCycles connected2"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles [[4,5,6,7,3],  [4,2,3],  [6,7]]
      , testCase "" "joinCycles connected3"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles [[4,5,6,7,3],  [4,2,3]]
      , testCase "" "joinCycles split"$       [S.fromList [2,3,4], S.fromList [6,7]] ~=? joinCycles [[4,2,3],  [6,7]]
       
      , testCase "" "basic graph analysis"$ test$ do
	 putStrLn "Printing result of basic cycle analysis:"
	 print$ pPrint (basicCycleAnalysis exampleGraph)
      
      ]

