module Types where

data Direction = U | D | L | R deriving (Show, Eq, Ord)

directions = [U, D, L, R]

class Updatable e where
  update :: e ()