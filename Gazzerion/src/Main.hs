import Card
import Field
import Draw
import GazWorld
import Header
import FreeGame
import Data.List
import System.Random
import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.State
update :: StateT World Game ()
update = do
    drawGrid =<< get
    drawbasement =<< get
    focusCardIndex <- drawhandcard =<< get
    maxp <- use maxplayer
    font <- use font
    hcards <- use handcards
    now <- use nowplayer
    ph <- use mode

    fld <- use field
    forM_ fld $ \f -> (f `drawTree`) =<< use enviroment

    case ph of
        Init -> do
            forM_ [0..maxp-1] $ \n -> do
                nowplayer .= n
                modify decktohand
            nowplayer .= 0
            modify updateField
            modify setCanPutsAndBurst
            --cpts <- use canputs--表示
            --embedIO $ print $ cpts--表示
            mode .= Choice
        Draw -> do
            modify decktohand
            modify setCanPutsAndBurst
            cpts <- use canputs--表示
            --embedIO $ print $ cpts--表示
            nowturn .= 0
            selectedhandcard .= -1
            if null $ concat cpts -- 詰み判定
                then mode .= Mate 0 ""
                else do
                    checkmatedcounter .= 0
                    mode .= Choice
        Choice -> do
            mb <- mouseButtonL
            when (mb && focusCardIndex /= Nothing) $ do
                mode .= Move False
                let takejust (Just n) = n
                selectedhandcard .= takejust focusCardIndex
        Move f -> do
                mw <- mouseScroll
                mb <- mouseButtonL
                when (not f && not mb) $ mode .= Move True
                nhk <- use selectedhandcard
                let nowhcard = hcards!!now
                    nowcard = nowhcard!!nhk
                    conlen = length (nowcard^.connector)
                nturn <- use nowturn
                cp <- use canputs
                when (mw^._y > 0) $ nowturn .= (nturn + 1) `mod` conlen
                when (mw^._y < 0) $ nowturn .= (nturn + conlen - 1) `mod` conlen
                drawCanput =<< get
                nowpt <- drawMovingCard =<< get
                when (mb && f && (Just nhk) == focusCardIndex) $ do
                    selectedhandcard .= -1
                    nowturn .= 0
                    mode .= Goto Choice
                when (mb && f && nowpt `elem` map snd (cp!!nhk)) $ do
                    let psg = fst $ (cp!!nhk)!!((elemIndices nowpt (map snd (cp!!nhk)))!!0) -- 置く場所のPassageID
                    fld <- use field
                    when (psg == -1) $ do
                        let newtree = initiation nowcard nowpt fld
                        field .= newtree
                        init <- use initiations
                        initiations .= (take now init) ++ [Just (hashtree $ head newtree)] ++ (drop (now + 1) init)
                    unless (psg == -1) $ field .= insertCard psg nowcard nturn fld
                    put =<< execStateT (dripHandcard nhk) =<< get -- 手札からカードををドロップ
                    hcards <- use handcards
                    deck <- use decks
                    if length (deck!!now) == 0 && length (hcards!!now) == 0
                        then mode .= GameOver
                        else mode .= Burst 0 []
                    modify updateField
                    --ff2 <- use field
                    --embedIO $ print ff2
        Mate count str
            | count == 0 -> do
                ckmate <- use checkmatedcounter
                if ckmate >= maxp -- 全詰み
                    then do
                        mode .= Mate 1 "All Checkmate"
                        checkmatedcounter .= 0
                        modify fieldclear -- 全流れ
                        modify setCanPutsAndBurst -- 置ける場所計算しなおし
                    else do
                        mode .= Mate 1 ((show (now +1)) ++ "P Checkmate")
                        checkmatedcounter += 1
            | count > 50 -> do
                modify nextplayer
                mode .= Draw
            | otherwise -> do
                (\w -> drawnotice w str blue count) =<< get
                mode .= Mate (count + 1) str
        Burst count bursttree
            | count == 0 -> do
                field <- use field
                if isburst field
                    then mode .= Burst 1 (burstfilter field)
                    else do
                        modify nextplayer
                        mode .= Draw
                modify burst
            | count > 50 -> mode .= Draw
            | otherwise -> do
                env <- use enviroment
                forM_ bursttree $ \t -> color red $ drawBurstTree t env count
                (\w -> drawnotice w ((show (now + 1)) ++ "P Burst") red count) =<< get
                mode .= Burst (count + 1) bursttree
        GameOver -> do
            color red $ translate (V2 300 450) $ text font 48 $ (show (now+1)) ++ "P WIN"
        Goto goto -> do
            mb <- mouseButtonL
            unless mb $ mode .= goto

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

