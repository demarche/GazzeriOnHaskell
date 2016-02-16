import Card
import Field
import Draw
import GazWorld
import Header
import FreeGame
import Control.Lens
import Data.List
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
    ph <- use mode

    fld <- use field
    forM_ fld $ \f -> drawTree f env

    case ph of
        Init -> do
            put =<< execStateT deckTohand =<< get
            put =<< execStateT updateField =<< get
            nowturn .= 0
            selectedhandcard .= -1
            mode .= Choice
        Choice -> do
            mb <- mouseButtonL
            when (selectedhk >= 0 && mb) $ mode .= Goto (Move False)
        Move f
            | not f -> do
                selectedhandcard .= selectedhk
                when (selectedhk >= 0) $ mode .= Move True
                when (selectedhk < 0) $ mode .= Choice
            | f -> do
                nhk <- use selectedhandcard
                let nowdeck = hcards!!now
                    nowcard = nowdeck!!nhk
                    conlen = length (nowcard^.connector)
                mw <- mouseScroll
                mb <- mouseButtonL
                nturn <- use nowturn
                cp <- use canputs
                nowpt <- drawMovingCard =<< get

                when (mw^._y > 0) $ nowturn .= (nturn + 1) `mod` conlen
                when (mw^._y < 0) $ nowturn .= (nturn + conlen - 1) `mod` conlen
                when (mb && nhk == selectedhk) $ do
                    selectedhandcard .= -1
                    nowturn .= 0
                    mode .= Goto Choice
                when (mb && nowpt `elem` map snd (cp!!nhk)) $ do
                    let psg = fst $ (cp!!nhk)!!((elemIndices nowpt (map snd (cp!!nhk)))!!0)
                    fld <- use field
                    when (psg == -1) $ do
                        field .= initiation nowcard nowpt fld
                        init <- use initiations
                        initiations .= (take now init) ++ [False] ++ (drop (now + 1) init)
                    unless (psg == -1) $ field .= insertCard psg nowcard nturn fld
                    handcards .= (take now hcards) ++ [(take nhk nowdeck) ++ (drop (nhk + 1) nowdeck)] ++ (drop (now + 1) hcards)
                    put =<< execStateT updateField =<< get
                    mode .= Goto Init
        Goto goto -> do
            mb <- mouseButtonL
            unless mb $ mode .= goto
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
