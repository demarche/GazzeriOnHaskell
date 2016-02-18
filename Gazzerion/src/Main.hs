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

modtest world = do
    return $ world&nowplayer.~0

update :: StateT World Game ()
update = do
    drawGrid =<< get
    selectedhk <-evalStateT drawPlayerCards =<< get
    font <- use font
    env <- use enviroment
    hcards <- use handcards
    now <- use nowplayer
    maxp <- use maxplayer
    ph <- use mode

    fld <- use field
    forM_ fld $ \f -> drawTree f env

    case ph of
        Init -> do
            forM_ [0..maxp-1] $ \n -> do
                nowplayer .= n
                modify decktohand
            nowplayer .= 0
            modify updateField
            modify setCanPutsAndBurst
            cpts <- use canputs--表示
            embedIO $ print $ cpts--表示
            mode .= Choice
        Draw -> do
            modify decktohand
            modify setCanPutsAndBurst
            cpts <- use canputs--表示
            embedIO $ print $ cpts--表示
            nowturn .= 0
            selectedhandcard .= -1
            if null $ concat cpts -- 詰み判定
                then do
                    ckmate <- use checkmatedcounter
                    if ckmate >= maxp -- 全詰み
                        then do
                            checkmatedcounter .= 0
                            modify fieldclear -- 全流れ
                            modify setCanPutsAndBurst -- 置ける場所計算しなおし
                        else checkmatedcounter += 1
                    modify nextplayer
                    mode .= Draw
                else do
                    checkmatedcounter .= 0
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
                let nowhcard = hcards!!now
                    nowcard = nowhcard!!nhk
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
                    let psg = fst $ (cp!!nhk)!!((elemIndices nowpt (map snd (cp!!nhk)))!!0) -- 置く場所のPassageID
                    fld <- use field
                    when (psg == -1) $ do
                        field .= initiation nowcard nowpt fld
                        init <- use initiations
                        initiations .= (take now init) ++ [False] ++ (drop (now + 1) init)
                    unless (psg == -1) $ field .= insertCard psg nowcard nturn fld
                    put =<< execStateT (dripHandcard nhk) =<< get -- 手札からカードををドロップ
                    hcards <- use handcards
                    deck <- use decks
                    embedIO $ print $ (show $ length $ deck!!now) ++ "," ++ (show $ length (hcards!!now))
                    if length (deck!!now) == 0 && length (hcards!!now) == 0
                        then mode .= GameOver
                        else mode .= Goto Burst
                    modify updateField
                    ff2 <- use field
                    embedIO $ print ff2
        Burst -> do
            field <- use field
            unless (isburst field) $ modify nextplayer
            modify burst
            mode .= Draw
        GameOver -> do
            color red $ translate (V2 300 450) $ text font 48 $ (show (now+1)) ++ "P WIN"
        Goto goto -> do
            --embedIO $ print "g"
            mb <- mouseButtonL
            unless mb $ mode .= goto
  where
    p0 = V2 390 320

mainLoop :: World -> Game ()
mainLoop s = do
  s' <- execStateT update s
  tick
  unlessM (keyDown KeyEscape) $ mainLoop s'

main = runGame Windowed (Box (V2 0 0) (V2 1440 900)) $ do
    font <- loadFont "VL-PGothic-Regular.ttf"
    --cd <- evalStateT randCard ()
    newW <- execStateT initDecks =<< execStateT updateEnv =<< return (makeWorld font)

    {-}forever $ do
      color red $ translate (V2 24 240) $ text font 24 "Press SPACE to start"
      tick
      whenM (keyDown KeySpace) $
      return ()-}
    mainLoop $ newW

