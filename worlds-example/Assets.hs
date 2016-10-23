module Assets where
import Types
import FreeGame
import System.IO.Unsafe

_Actor_png = unsafePerformIO $ readBitmap "Actor.png"

characterBitmap :: Int -> Int -> Bitmap -> Int -> Direction -> Bitmap
characterBitmap w h b n d = cropBitmap b (w, h) (w * j, k * h) where
  j = case n `mod` 4 of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 1
  k = case d of
    D -> 0
    L -> 1
    R -> 2
    U -> 3

playerBitmap :: Int -> Direction -> Bitmap
playerBitmap = characterBitmap 32 32 _Actor_png