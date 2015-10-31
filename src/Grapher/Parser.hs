{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Grapher.Parser (parseDot) where

import Data.Bifunctor
import Data.List
import Data.Maybe
import Language.Dot.Syntax
import qualified Language.Dot.Parser as P

parseDot :: String -> Either String (Int, [(Int, Int)])
parseDot = bimap show discardGrahpvizInfo . P.parseDot "Grapher.Parser.parseDot"

discardGrahpvizInfo :: Graph -> (Int, [(Int, Int)])
discardGrahpvizInfo (Graph _ _ _ stmnts) = (length nodes, map (bimap ix ix) edges)
  where
    ix :: String -> Int
    ix = fromJust . (`elemIndex` nodes)

    nodes :: [String]
    nodes = mapMaybe statementnode stmnts

    edges :: [(String, String)]
    edges = concatMap statmentedge stmnts

    statementnode = \case
      NodeStatement nodeid _ -> nodename nodeid
      _ -> Nothing

    statmentedge = \case
      EdgeStatement entities _ -> groupedges (mapMaybe edgeid entities)
      _ -> []

    nodename (NodeId (NameId str) _)    = Just str
    nodename (NodeId (StringId str) _)  = Just str
    nodename (NodeId (IntegerId int) _) = Just (show int)
    nodename _                          = Nothing

    edgeid (ENodeId _ nodeid) = nodename nodeid
    edgeid _                  = Nothing

    groupedges []       = []
    groupedges [_]      = [] -- This is an error really
    groupedges (a:b:xs) = (a, b) : groupedges xs
