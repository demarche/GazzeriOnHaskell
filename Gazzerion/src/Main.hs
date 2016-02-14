{-# LANGUAGE TemplateHaskell #-}
import Card
import Field
import Draw
import GazWorld
import FreeGame
import Control.Lens
import System.Random
import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.Trans.Maybe

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

speed = 1

update :: StateT World Game ()
update = do
    drawGrid =<< get
    drawPlayerCards =<< get

    s0 <- use seq0
    s1 <- use seq1
    ofs <- use offset
    t <- use target
    font <- use font
    env <- use enviroment

    now <- use nowplayer
    let v = s0 !! t + s1 !! t
        card = ModCard {_connector = [4, 1, 2, 3], _image = 0, _size = Size 4 6}
        tree = initiation card 0
        tree2 = insertCard 1 card 1 tree
        tree3 = insertCard 6 card 1 tree2
        tree4 = refreshTree tree3 [tree3] (Size 6 4)
    drawTree tree4 env
    color black $ do
      translate (V2 24 240) $ text font 24 "fibs"
      translate (V2 24 280) $ text font 24 "tail fibs"
      forM_ (zip [0..] s0) $ \(i, v) -> translate (ofs + V2 (i * 36) 240) $ text font 24 (show v)
      forM_ (zip [0..] s1) $ \(i, v) -> translate (ofs + V2 (i * 36) 280) $ text font 24 (show v)
    color blue $ line [V2 400 480, V2 400 0]
    ph <- use mode
    case ph of
      Init -> do
        maxhcards <- use maxhandcards
        deckis <- use decks
        hk <- use handcards
        let nowhk = hk!!now
            nowdk = deckis!!now
            drawnum = maxhcards!!now - length nowhk -- ドローする枚数
        newhk <- flip execStateT hk $ ix now .= nowhk ++ take drawnum nowdk
        handcards .= newhk
        newdk <- flip execStateT deckis $ ix now .= drop drawnum nowdk
        decks .= newdk
        mode .= Scroll 10
      Scroll ph
        | ph > 0 -> do
          mode .= Scroll (ph - 1)

          color black $ translate (V2 390 320) $ text font 24 (show v)
          offset .= ofs - V2 speed 0
        | otherwise -> mode .= Dist 0
      Dist ph
        | ph >= 1 -> do
          seq0 .= s0 ++ [v]
          seq1 .= s1 ++ [v]
          mode .= Scroll 36
          target += 1
        | otherwise -> color black $ do
          translate (p0 ^* (1 - ph) + V2 (390+36) 240 ^* ph) $ text font 24 (show v)
          translate (p0 ^* (1 - ph) + V2 (390) 280 ^* ph) $ text font 24 (show v)
          mode .= Dist (ph + 1/30)
    color red $ translate (V2 24 120) $ text font 24 "Press ESC to exit"
  where
    p0 = V2 390 320

mainLoop :: World -> Game ()
mainLoop s = do
  s' <- execStateT update s
  tick
  unlessM (keyDown KeyEscape) $ mainLoop s'
{-
main = do
    rnd1 <- randCard
    rnd2 <- randCard
    rnd3 <- randCard
    let tree = initiation rnd1 0
        tree2 = insertCard 1 rnd2 1 tree
        tree3 = insertCard 6 rnd3 1 tree2
        tree4 = refreshTree tree3 [tree3] (Size 6 4)

    print $ getCanPutPassageIDs rnd1 1 [tree4]
    print tree4-}


main = runGameDefault $ do
    font <- loadFont "VL-PGothic-Regular.ttf"
    --cd <- evalStateT randCard ()
    newW <-execStateT initDecks =<< execStateT updateEnv =<< return (makeWorld font)
    {-runMaybeT $ forever $ do
      color red $ translate (V2 24 240) $ text font 24 "Press SPACE to start"
      tick
      () <- whenM (keyDown KeySpace) mzero
      return ()-}
    mainLoop $ newW
