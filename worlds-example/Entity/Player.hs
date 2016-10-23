{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Entity.Player where
import Include
import Types
import Assets

data States = States
  { _hp :: Float
  , _position :: Vec2
  , _direction :: Direction
  , _animation :: Int
  }
makeLenses ''States

data Actions x where
  GetCoord :: Actions Vec2
  Update :: Actions ()
  Move :: Direction -> Actions ()

instance Updatable Actions where
  update = Update

type Player s = Life (WorldT s Game) Actions ()

handle :: (MonadState States m, FreeGame m) => Actions a -> m a
handle GetCoord = use position
handle Update = do
  d <- use direction
  n <- use animation
  p <- use position
  translate p $ bitmap $ playerBitmap (n `div` 5) d
handle (Move d) = do
  case d of
    L -> position -= V2 2 0
    R -> position += V2 2 0
    U -> position -= V2 0 2
    D -> position += V2 0 2
  direction .= d
  animation += 1

life :: States -> Player s
life s = Alive $ \e -> do
  (a, s') <- lift $ runStateT (handle e) s
  return (a, life s')

new :: Vec2 -> Player s
new p = life $ States
  { _hp = 8
  , _position = p
  , _direction = R
  , _animation = 0
  }
