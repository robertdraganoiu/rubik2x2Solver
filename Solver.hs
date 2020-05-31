{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Solver where
import ProblemState
import Data.Maybe

--- state, action, depth, parent, successors
data Node s a = MakeNode { nodeState :: s 
                         , nodeAction :: Maybe a
                         , nodeDepth :: Int 
                         , nodeParent :: Maybe (Node s a)
                         , nodeChildren :: [Node s a]
                         }


------- State space functions -------
createStateSpaceAux :: (ProblemState s a, Eq s) => (a, s) -> Int -> Node s a -> Node s a
createStateSpaceAux (a, s) depth parent = current
    where current = MakeNode s (Just a) (depth + 1) (Just parent) children
          children = [createStateSpaceAux el (depth + 1) current | el <- filteredSuccessors]
          filteredSuccessors = [(succA, succS) | (succA, succS) <- (successors s), succS /= (nodeState parent), not (sameType a succA)]

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace sRoot = root
    where root = MakeNode sRoot Nothing 0 Nothing children
          children = [createStateSpaceAux el 0 root | el <- (successors sRoot)]


------- Bfs functions -------
bfsStream :: Eq s => [s] -> [Node s a] -> [([Node s a], [Node s a])]
bfsStream visited frontier = (discoveredNodes, newFrontier) : (bfsStream newVisited newFrontier)
    where newFrontier = tail frontier ++ discoveredNodes
          newVisited = foldr (:) visited [nodeState n | n <- discoveredNodes]
          discoveredNodes = [n | n <- (nodeChildren (head frontier)), not (elem (nodeState n) visited)]

-- adds bfs for all children to flux, followed by the results of nextStep bfs
bfs :: Eq s => Node s a -> [([Node s a], [Node s a])]
bfs node = (childr, childr) : (bfsStream childrStates childr)
    where childrStates = [nodeState child | child <- childr]
          childr = (nodeChildren node)


------- Bidir bfs functions -------
-- extracts pair of nodes from solution
solution :: Eq s => (([Node s a], [Node s a]), ([Node s a], [Node s a])) -> (Node s a, Node s a)
solution ((_, fr1), (_, fr2)) = head [(n1, n2) | n1 <- fr1, n2 <- fr2, (nodeState n1) == (nodeState n2)]

-- verifies frontier intersection
notMatch :: Eq s => (([Node s a], [Node s a]), ([Node s a], [Node s a])) -> Bool
notMatch ((_, fr1), (_, fr2)) = not $ (length [(n1, n2) | n1 <- fr1, n2 <- fr2, (nodeState n1) == (nodeState n2)]) > 0

-- drops first element until frontiers intersect
dropUntilMatch :: Eq s => [(([Node s a], [Node s a]), ([Node s a], [Node s a]))] -> [(([Node s a], [Node s a]), ([Node s a], [Node s a]))]
dropUntilMatch streams = dropWhile notMatch streams

bidirBFS :: Eq s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start win = solution $ head $ dropUntilMatch streams
    where streams = zip (bfs start) (bfs win) 


------- Path functions -------
extractPath :: Node s a -> [(Maybe a, s)]
extractPath win = [(a, nodeState n) | (a, n) <- updatedLst] -- get (action, state) list
    where updatedLst = (Nothing, (fromMaybe win (nodeParent (snd (head lst))))) : lst -- add first element manually
          lst = reverse $ takeWhile hasParent $ iterate getNextPair ((nodeAction win), win) -- compute list of (action, node)
          getNextPair (_, node) = ((nodeAction (fromMaybe win (nodeParent node))), fromMaybe win (nodeParent node)) -- iterate func
          hasParent ((Just _), _) = True
          hasParent (Nothing, _) = False

reversePath :: (ProblemState s a, Eq s) => [(Maybe a, s)] -> [(Maybe a, s)]
reversePath path = reverse $ tail $ map reversePathAux path
    where reversePathAux (Nothing, s) = (Nothing, s)
          reversePathAux (Just a, s) = toJust' $ reverseAction (a, s)
          toJust' (a, s) = (Just a, s)

solve :: (ProblemState s a, Eq s) => s -> [a]
solve start = (map fromJust . tail . map fst) (startPath ++ winPath)
    where startPath = extractPath start_inter
          winPath = reversePath $ extractPath win_inter
          (start_inter, win_inter) = bidirBFS (createStateSpace start) (createStateSpace (createFinal start))