module GazWorld where
import Control.Applicative
import Data.Maybe
import Card
import Header
import FreeGame
import Field
import Control.Lens
import Control.Monad.State

makeWorld font = World Init (Size 4 6) (Size 4 6) font (Size 18 24) (Enviroment 0 0 0 0 0) (Size 1440 900) 2 0 [100, 100] [3, 3] [3, 3] [[], []] [[], []] [[], []] [Nothing, Nothing] (-1) 0 [] [] 0 2

-- 大域的な環境の更新
-- グリッドのサイズ、開始位置
updateEnv :: StateT World Game ()
updateEnv = do
    fs <- use fieldsize
    cs <- use cardsizeMax
    ss <- use screensize
    let mygrid = ss^.height `div` (fs^.height + cs^.height * 2 + 4)
        gridY = mygrid * (cs^.height + 2)
        gridX = (ss^.width - mygrid * fs^.width) `div` 2
    enviroment .= Enviroment mygrid gridX gridY (mygrid * fs^.width) (mygrid * fs^.height)

-- ランダムデッキ作成
initDecks :: StateT World Game ()
initDecks = do
    dmax <- use deckmax
    let getDeck n = replicateM n $ embedIO $ randCard2
    dks <- forM dmax $ \x -> getDeck x
    decks .= dks

-- デッキからドロー
decktohand :: World -> World
decktohand world = (world&decks.~newdecks)&handcards.~newhcards where
    now = world^.nowplayer
    dks = world^.decks
    hcards = world^.handcards
    drawnum = (world^.maxhandcards)!!now - length (hcards!!now) -- ドローする枚数
    newdecks = (take now dks) ++ [drop drawnum (dks!!now)] ++ (drop (now+1) dks)
    newhcards = (take now hcards) ++ [(hcards!!now) ++ (take drawnum (dks!!now))] ++ (drop (now+1) hcards)

-- バースト
burst :: World -> World
burst world = let hashes = map (\y -> Just $ hashtree y) $ filter (\x ->  burstCounter x /= Nothing) (world^.field) -- バーストする木のハッシュ
                  newfield = filter (\x ->  burstCounter x == Nothing) (world^.field)
                  newinit = [if init `elem` hashes then Nothing else init | init <- world^.initiations]
    in (world&field.~newfield)&initiations.~newinit

-- バーストフラグ
isburst field = or [burstCounter t /= Nothing | t <- field]

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

--全流れ
fieldclear world = setCanPutsAndBurst $ (world&field.~[])&initiations.~replicate (world^.maxplayer) Nothing

--次のプレイヤーへ
nextplayer :: World -> World
nextplayer world = world&nowplayer.~((1 + world^.nowplayer) `mod` world^.maxplayer)

--現在のプレイヤーのn番目のカード
nofhcard world n = (world^.handcards)!!(world^.nowplayer)!!n

--手札からn枚目のカードをドリップ
dripHandcard :: Int -> StateT World (StateT World Game) ModCard
dripHandcard n = do
    now <- use nowplayer
    hcards <- use handcards
    let nowhcard = hcards!!now
    handcards .= (take now hcards) ++ [(take n nowhcard) ++ (drop (n + 1) nowhcard)] ++ (drop (now + 1) hcards)
    nowcard <- (`nofhcard` n) <$> get
    return nowcard