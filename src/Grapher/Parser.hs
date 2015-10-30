{-# LANGUAGE OverloadedStrings #-}
module Grapher.Parser (parseGraph, toGraph) where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import Data.Char
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import qualified Data.Set as S
import qualified Data.Text as T

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy _ [] = ([], [])
splitBy p (x:xs)
  | p x       = ([], xs)
  | otherwise = (x:ys, zs)
  where
    (ys, zs) = splitBy p xs

data Direction = N | NE | E | SE | S | SW | W | NW | U | D
  deriving Show

data Node = Node { name :: Text, label :: Text }
  deriving Show

data Edge = Edge { from :: Text, to :: Text, direction :: Direction }
  deriving Show

identifier :: Parser Text
identifier = P.takeTill isSpace

parseDirection :: Parser Direction
parseDirection =
  ("NE" *> pure NE) <|>
  ("NW" *> pure NW) <|>
  ("SE" *> pure SE) <|>
  ("SW" *> pure SW) <|>
  ("E"  *> pure E) <|>
  ("N"  *> pure N) <|>
  ("S"  *> pure S) <|>
  ("W"  *> pure W) <|>
  ("U"  *> pure U) <|>
  ("D"  *> pure D)

parseNode :: Parser Node
parseNode = Node <$> (identifier <* P.skipSpace) <*> P.takeText

parseEdge :: Parser Edge
parseEdge = Edge <$> (identifier <* P.skipSpace) <*> (identifier <* P.skipSpace) <*> parseDirection

parseGraph :: Text -> Either String ([Node], [Edge])
parseGraph txt = do
  nodes <- mapM (P.parseOnly parseNode) ns
  edges <- mapM (P.parseOnly parseEdge) es
  return (nodes, edges)
  where
    notEmpty = not . T.null
    notComment = not . T.isPrefixOf "//"

    ls = filter (\l -> notEmpty l && notComment l) $ T.lines txt

    (ns, es) = splitBy (=="#") ls

toGraph :: ([Node], [Edge]) -> (Int, [(Int, Int)])
toGraph (nodes, edges) = (n, es)
  where
    n = length nodes

    ns = S.fromList (map name nodes)
    es = concatMap edge edges

    edge e = let i = S.findIndex (from e) ns
                 j = S.findIndex (to e) ns
              in [(i,j), (j,i)]
