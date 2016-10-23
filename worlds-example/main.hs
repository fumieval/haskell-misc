import Include
import Types
import Assets
import qualified Entity.Player as Player
import Control.Monad.One

haunt :: (Monad m, FreeGame m) => Name s Player.Actions r -> Life (WorldT s m) Identity ()
haunt she = go R where
  go d = Alive $ \(Identity pass) -> do
    r <- lift $ randomness (0 :: Int, 59)
    she .! Player.Move d
    if r == 0 then do
      i <- lift $ randomness (0, 3)
      return (pass, go (directions !! i))
    else return (pass, go d)

main = runGameDefault $ runWorldT $ do
    player <- spawn $ Player.new (V2 240 240)
    playerClone <- spawn $ Player.new (V2 320 240)
    doppelganger <- spawn $ haunt playerClone
    forever $ do
        whenM (lift $ keyPress KeyLeft)    $ player .! Player.Move L
        whenM (lift $ keyPress KeyRight)   $ player .! Player.Move R
        whenM (lift $ keyPress KeyDown)    $ player .! Player.Move D
        whenM (lift $ keyPress KeyUp)      $ player .! Player.Move U
        player .! update
        playerClone .! update
        doppelganger .! return ()
        lift tick