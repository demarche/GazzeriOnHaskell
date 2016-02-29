module GazWorld where
import Control.Applicative
import Data.Maybe
import Card
import Draw
import Header
import FreeGame
import Field
import Control.Lens
import Control.Monad.State
import Magic.Load
import Magic.CanUse
import System.Directory
import System.FilePath.Posix

makeWorld font = World Init (Size 4 6) (Size 4 6) font (Size 18 24) (Enviroment 0 0 0 0 0 []) (Size 1440 900) 2 0 [100, 100] [3, 3] [3, 3] [[], []] [[], []] [[], []] [Nothing, Nothing] (-1) (-1) 0 [] [] [] 0 2 [2, 2]

-- 大域的な環境の更新
-- グリッドのサイズ、開始位置
updateEnv :: StateT World Game ()
updateEnv = do
    fs <- use fieldsize
    cs <- use cardsizeMax
    ss <- use screensize
    env <- use enviroment
    maxh <- use maxhandcards
    maxm <- use maxmagiccards
    maxp <- use maxplayer
    let mygrid = ss^.height `div` (fs^.height + cs^.height * 2 + 4) -- グリッドサイズ
        gridY = mygrid * (cs^.height + 2) -- グリッド描画開始位置X
        gridX = (ss^.width - mygrid * fs^.width) `div` 2 -- グリッド描画開始位置Y
        gridWid = (mygrid * fs^.width)
        gridHei = (mygrid * fs^.height)
        getCardsWidth n = mygrid * (cs^.width * (maxh!!n + maxm!!n + 1) + 4) -- n番目のプレイヤーのカードの総width
        synthesisWidth x = sum [getCardsWidth i | i <- [0..maxp-1], i `mod` 4 == x] -- n番テーブルの総width
        synthesisHeight = mygrid * (2 + cs^.height)
        playerstartPosition n = case n `mod` 4 of
            0 -> (v2Int ((ss^.width - synthesisWidth n) `div` 2 + n `div` 4 * getCardsWidth n) (gridY + gridHei), v2Int (getCardsWidth n) synthesisHeight)
            1 -> (v2Int (ss^.width - (ss^.width - synthesisWidth n) `div` 2 - n `div` 4 * getCardsWidth n) synthesisHeight, v2Int (getCardsWidth n) synthesisHeight)
    enviroment .= Enviroment mygrid gridX gridY gridWid gridHei [playerstartPosition n | n <- [0..maxp-1]]

-- フィールド更新
updateField :: World -> World
updateField world =
    let f1 = map propCoordinate (world^.field)
        f2 = [appDeadend t1 (world^.cardsizeMin) (world^.fieldsize) f1 | t1 <- f1]
        f' = filter (\x -> case x of Fork h -> True; _ -> False) f2
    in world&field.~f'

-- ランダムデッキ作成
initDecks :: StateT World Game ()
initDecks = do
    dmax <- use deckmax
    mmax <- use maxmagiccards
    onepair <- lift $ loadMagicAll
    let mcs = replicate 2 onepair
    --embedIO $ print allmagic
    let getDeck n = replicateM n $ embedIO $ randCard2
    dks <- mapM getDeck dmax--forM dmax $ \x -> getDeck x
    decks .= dks
    magiccards .= mcs
    maxmagiccards .= map length mcs

-- デッキからドロー
decktohand :: World -> World
decktohand world =
    let now = world^.nowplayer
        dks = world^.decks
        hcards = world^.handcards
        drawnum = (world^.maxhandcards)!!now - length (hcards!!now) -- ドローする枚数
        newdecks = (take now dks) ++ [drop drawnum (dks!!now)] ++ (drop (now+1) dks)
        newhcards = (take now hcards) ++ [(hcards!!now) ++ (take drawnum (dks!!now))] ++ (drop (now+1) hcards)
    in (world&decks.~newdecks)&handcards.~newhcards

-- バースト
burst :: World -> World
burst world =
    let hashes = map (\y -> hashtree y) $ filter (\x ->  burstCounter x /= Nothing) (world^.field) -- バーストする木のハッシュ
        bursted = filter (\x ->  burstCounter x == Nothing) (world^.field)
        newfield = unDeadEnd bursted
        now = world^.nowplayer
        newcosts = take now (world^.costs) ++ [(world^.costs)!!now + sum [numofTreeCard x | x <- world^.field, burstCounter x /= Nothing]] ++ drop (now + 1) (world^.costs)
    in (`resetinit` hashes) $ updateField $ (world&field.~newfield)&costs.~newcosts

-- イニシエーションリセット
resetinit :: World -> [Int] -> World
resetinit world hashes = world&initiations.~ map (\x -> case x of Just y -> if y `elem` hashes then Nothing else x ; Nothing -> x) (world^.initiations)

-- 魔法の使用可能状態をセット
setCanMagic :: World -> World
setCanMagic world =
    let canusemagic = zipWith (\x now -> (\card -> canuseMagic card world && card^.cost <= (world^.costs)!!now) `map` x) (world^.magiccards) [0..]
    in world&canmagic.~canusemagic

-- 手札の置ける場所をセット（低バースト考慮）
setCanPutsAndBurst :: World -> World
setCanPutsAndBurst world = includeBurst where
    excludeBurst = setCanPuts world
    includeBurst = unsetCantPutsByLowBurst excludeBurst

-- 手札の置ける場所をセット（低バースト未考慮）
setCanPuts :: World -> World
setCanPuts world = world&canputs.~[initputs crd ++ normalputs crd | crd <- (world^.handcards)!!(world^.nowplayer)] where
    normalputs = \crd -> concat $ [canput crd trn tree world | tree <- (world^.field), trn <- [0..length (crd^.connector)-1]] -- イニシエーション以外のおける場所
    initputs = \crd -> if (world^.initiations)!!(world^.nowplayer) == Nothing -- イニシエーション可能の時だけイニシエーションラインの置ける場所をチェック
        then concat $ [canputInit crd trn world | trn <- [0..length (crd^.connector)-1]]
        else []

-- 手札の置ける場所から低バーストするものを取り除く
unsetCantPutsByLowBurst :: World -> World
unsetCantPutsByLowBurst world = world&canputs.~[filter (`ishighburst` (nofhcard world n)) ((world^.canputs)!!n) | n <- [0..length (world^.canputs)-1]] where
    ishighburstfield wld = null $ filter (<= wld^.lowburst) $ mapMaybe burstCounter (wld^.field)
    ishighburst (p, st) card = (ishighburstfield . updateField) $ world&field.~ if p < 0
        then initiation card st (world^.field)
        else insertCard p card (st^.turn) (world^.field)

-- 全流れ
fieldclear :: World -> World
fieldclear world = setCanPutsAndBurst $ (world&field.~[])&initiations.~replicate (world^.maxplayer) Nothing

-- 次のプレイヤーへ
nextplayer :: World -> World
nextplayer world = world&nowplayer.~((1 + world^.nowplayer) `mod` world^.maxplayer)

-- 現在のプレイヤーのn番目のカード
nofhcard world n = (world^.handcards)!!(world^.nowplayer)!!n

-- 手札からn枚目のカードをドリップ
dripHandcard :: Int -> StateT World (StateT World Game) ModCard
dripHandcard n = do
    now <- use nowplayer
    hcards <- use handcards
    let nowhcard = hcards!!now
    handcards .= (take now hcards) ++ [(take n nowhcard) ++ (drop (n + 1) nowhcard)] ++ (drop (now + 1) hcards)
    nowcard <- (`nofhcard` n) <$> get
    return nowcard

-- 選択中の魔法カードを使用
removeNowMagic :: World -> World
removeNowMagic world =
    let mindex = world^.selectedmagiccard
        mcards = world^.magiccards
        now = world^.nowplayer
        newcosts = take now (world^.costs) ++ [(world^.costs)!!now - ((mcards!!now!!mindex)^.cost)] ++ drop (now + 1) (world^.costs)
    in ((world&magiccards .~ (take now mcards) ++ [take mindex (mcards!!now) ++ drop (mindex + 1) (mcards!!now)] ++ (drop (now + 1) mcards))&selectedmagiccard .~ -1)&costs.~ newcosts