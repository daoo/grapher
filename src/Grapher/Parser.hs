{-# LANGUAGE BangPatterns, LambdaCase, OverloadedStrings #-}
module Grapher.Parser (parseDot) where

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Tuple
import Language.Dot.Syntax
import qualified Data.Map as M
import qualified Language.Dot.Parser as P

parseDot :: String -> Either String (Int, [(Int, Int)])
parseDot = bimap show discardGrahpvizInfo . P.parseDot "Grapher.Parser.parseDot"

discardGrahpvizInfo :: Graph -> (Int, [(Int, Int)])
discardGrahpvizInfo (Graph _ _ _ stmnts) = nodecount $ nub $ sort $ edges stmnts
  where
    nodecount :: [(String, String)] -> (Int, [(Int, Int)])
    nodecount = bimap fst id . mapAccumL f (0 :: Int, M.empty)
      where
        f !acc (a, b) = (acc'', (i, j))
          where
            (acc', i)  = g acc a
            (acc'', j) = g acc' b

        g (count, m) a =
          case M.lookup a m of
            Nothing -> ((count+1, M.insert a count m), count)
            Just i  -> ((count, m), i)

    edges :: [Statement] -> [(String, String)]
    edges = concatMap f
      where
        f :: Statement -> [(String, String)]
        f = \case
          NodeStatement _ _ -> []
          EdgeStatement entities _ -> duplicate (pair (mapMaybe edgeid entities))
          AttributeStatement _ _ -> []
          AssignmentStatement _ _ -> []
          SubgraphStatement _ -> []

    nodename :: NodeId -> Maybe String
    nodename (NodeId (NameId str) _) = Just str
    nodename (NodeId (StringId str) _) = Just str
    nodename (NodeId (IntegerId int) _) = Just (show int)
    nodename (NodeId (FloatId flt) _) = Just (show flt)
    nodename (NodeId (XmlId _) _) = Nothing

    edgeid :: Entity -> Maybe String
    edgeid (ENodeId _ nodeid) = nodename nodeid
    edgeid (ESubgraph _ _) = Nothing

pair :: [a] -> [(a,a)]
pair []       = []
pair [_]      = [] -- This is an error
pair (a:b:xs) = (a, b) : pair (b : xs)

duplicate :: [(a, a)] -> [(a, a)]
duplicate = concatMap (\x -> [x, swap x])
