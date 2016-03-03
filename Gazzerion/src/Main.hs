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
import Magic.Func

takejust (Just n) = n

update :: StateT World Game ()
update = do
    drawGrid =<< get
    drawbasement =<< get
    focusCardIndex <- drawhandcard =<< get
    focusMagicIndex <- drawhandmagiccard =<< get
    maxp <- use maxplayer
    font <- use font
    hcards <- use handcards
    now <- use nowplayer
    ph <- use mode

    mb <- mouseButtonL
    mw <- mouseScroll
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
            modify setCanMagic
            mode .= Choice
        Draw -> do
            modify decktohand
            nowturn .= 0
            selectedhandcard .= -1
            mode .= MateCheck
        MateCheck -> do
            modify setCanPutsAndBurst
            modify setCanMagic
            cpts <- use canputs
            cmagic <- use canmagic
            if null (concat cpts) && not (or (cmagic!!now))
                then mode .= Mate 0 ""
                else do
                    mode .= Choice
        Choice -> do
            when (mb && focusCardIndex /= Nothing) $ do
                checkmatedcounter .= 0
                mode .= Move False
                selectedhandcard .= takejust focusCardIndex
            when (mb && focusMagicIndex /= Nothing) $ do
                checkmatedcounter .= 0
                let mindex = takejust focusMagicIndex
                mcards <- use magiccards
                selectedmagiccard .= mindex
                mode .= Goto (Magic ((mcards!!now!!mindex)^.funcs) (Interpreter []))
            cpts <- use canputs
            when (null $ concat cpts) $ do
                (\w -> drawnotice w "S:スキップ" red 0) =<< get
                key <- keyChar 'S'
                when key $ mode .= Mate 0 ""
        Magic inst mph -> case mph of
            Interpreter arg -> do
                if null inst
                    then mode .= Magic [] End
                    else do
                        let myinst = head inst
                        mode .= Magic (drop 1 inst) mph
                        let myfunc = interpreterMagic $ myinst
                        modify myfunc
            SelectFieldCard num res -> do
                focusFieldHash <- selectedCardHash =<< get
                get >>= \y -> mapM_ (\x -> glowCardGHash x y) res
                when (mb && focusFieldHash /= Nothing) $ do
                    let arged = nub $ (takejust focusFieldHash):res
                    if length arged >= num
                        then mode .= Magic inst (Interpreter arged)
                        else mode .= Magic inst (SelectFieldCard num arged)
            SelectField arg card canburst -> do
                nturn <- use nowturn
                cursor <- (`drawFollowingCard` card) =<< get
                when (mw^._y > 0) $ nowturn .= (nturn + 1) `mod` 4
                when (mw^._y < 0) $ nowturn .= (nturn + 3) `mod` 4
                when mb $ do
                    fld <- use field
                    fsize <- use fieldsize
                    low <- use lowburst
                    minsize <- use cardsizeMin
                    let newtree = initiation card cursor fld
                        gethub (Fork h) = h
                        refleshed = map (\x -> appDeadend x minsize fsize newtree) newtree; highhashes = [hashtree x | x <- newtree, not $ isLowburstTree x low]
                        ignoreLowBurst = filter (\x -> elem (hashtree x) highhashes) refleshed -- refleshedから低バーストのものを除いたもの
                    when (not (isCollision (gethub (head newtree)) fld) && not (fieldover cursor (card^.size) fsize) && and [burstCounter t == Nothing || if canburst then numofTreeCard t > low else False| t <- ignoreLowBurst]) $ do
                        field .= refleshed
                        if isburst refleshed low
                            then mode .= Burst 0 [] (Magic inst (Interpreter arg))
                            else mode .= Magic inst (Interpreter arg)
            End -> do
                modify removeNowMagic
                mode .= MateCheck
        Move f -> do
            when (not f && not mb) $ mode .= Move True
            nhk <- use selectedhandcard
            maxh <- use maxhandcards
            let nowhcard = hcards!!now
                nowcard = nowhcard!!nhk
                conlen = length (nowcard^.connector)
            nturn <- use nowturn
            cp <- use canputs
            when (mw^._y > 0) $ nowturn .= (nturn + 1) `mod` conlen
            when (mw^._y < 0) $ nowturn .= (nturn + conlen - 1) `mod` conlen
            when (nhk < maxh!!now) $drawCanput =<< get
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
                num <- use numofput
                if length (deck!!now) == 0 && length (hcards!!now) == 0
                    then  mode .= GameOver
                    else if num == 1 then mode .= Burst 0 [] Draw else do
                        nowturn .= 0
                        selectedhandcard .= -1
                        numofput .= num - 1
                        mode .= MateCheck
                modify updateField
        Mate count str
            | count == 0 -> do
                ckmate <- use checkmatedcounter
                if ckmate + 1 == maxp -- 全詰み
                    then do
                        mode .= Mate 1 "All Checkmate"
                        checkmatedcounter .= 0
                        modify fieldclear -- 全流れ
                        modify setCanPutsAndBurst -- 置ける場所計算しなおし
                    else do
                        mode .= Mate 1 ((show (now +1)) ++ "P Checkmate")
                        checkmatedcounter += 1
            | count > 50 -> do
                mode .= Draw
                unless (elem str ["All Checkmate"]) $ modify nextplayer
            | otherwise -> do
                (\w -> drawnotice w str blue count) =<< get
                mode .= Mate (count + 1) str
        Burst count bursttree next
            | count == 0 -> do
                field <- use field
                low <- use lowburst
                if isburst field low
                    then mode .= Burst 1 (burstfilter field low) next
                    else do
                        modify nextplayer
                        mode .= next
                modify burst
            | count > 50 -> mode .= next
            | otherwise -> do
                env <- use enviroment
                forM_ bursttree $ \t -> color red $ drawBurstTree t env count
                (\w -> drawnotice w ((show (now + 1)) ++ "P Burst") red count) =<< get
                mode .= Burst (count + 1) bursttree next
        GameOver -> do
            color red $ translate (V2 300 450) $ text font 48 $ (show (now+1)) ++ "P WIN"
        PError -> do
            color red $ translate (V2 300 450) $ text font 48 "Phase Error"
        Goto goto -> do
            mb <- mouseButtonL
            unless mb $ mode .= goto

mainLoop :: World -> Game ()
mainLoop s = do
  s' <- execStateT update s
  tick
  unlessM (keyDown KeyEscape) $ mainLoop s'

initDecks2 :: StateT World Game ()
initDecks2 = do
    dmax <- use deckmax
    let getDeck = [card_con [0,3,0,0], card_con [3,1,5,1], card_con [0,1,2,1], card_con [0,0,0,1], card_con [4,0,3,2], card_con [4,4,4,4], card_con [3,3,3,3]]
        getDeck2 = [card_con [2,0,3,0], card_con [3,0,3,1], card_con [3,0,0,2], card_con [1,4,0,4], card_con [0,4,0,4], card_con [4,4,4,4], card_con [3,3,3,3]]
    decks .= [getDeck, getDeck2]

main = runGame Windowed (Box (V2 0 0) (V2 1440 900)) $ do
    font <- loadFont "VL-PGothic-Regular.ttf"
    --cd <- evalStateT randCard ()
    newW <- execStateT updateEnv =<< execStateT initDecks =<< return (makeWorld font)

    {-}forever $ do
      color red $ translate (V2 24 240) $ text font 24 "Press SPACE to start"
      tick
      whenM (keyDown KeySpace) $
      return ()-}
    mainLoop $ newW

