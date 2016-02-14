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
    selectedhk <-evalStateT drawPlayerCards =<< get
    font <- use font
    env <- use enviroment
    hcards <- use handcards

    now <- use nowplayer
    let card = ModCard {_connector = [4, 1, 2, 3], _image = 0, _size = Size 4 6}
        tree = initiation card 0
        tree2 = insertCard 1 card 1 tree
        tree3 = insertCard 6 card 1 tree2
        tree4 = refreshTree tree3 [tree3] (Size 6 4)
    drawTree tree4 env
    ph <- use mode
    case ph of
      Init -> do
        put =<< execStateT deckTohand =<< get
        mode .= Choice
      Choice -> do
        mb <- mouseButtonL
        when (selectedhk >= 0 && mb) $  mode .= Move selectedhk
      Move nhk -> do
        let nowcard = (hcards!!now)!!nhk
        mp <- mousePosition
        translate (mp - 0.5 * (v2Int (env^.grid * nowcard^.size^.width) (env^.grid * nowcard^.size^.height))) $ drawCard nowcard env
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
    newW <- execStateT initDecks =<< execStateT updateEnv =<< return (makeWorld font)

    {-runMaybeT $ forever $ do
      color red $ translate (V2 24 240) $ text font 24 "Press SPACE to start"
      tick
      () <- whenM (keyDown KeySpace) mzero
      return ()-}
    mainLoop $ newW
