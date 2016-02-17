module GazWorld where
import Card
import Header
import FreeGame
import Field
import Control.Lens
import Control.Monad.State

makeWorld font = World Init (Size 4 6) (Size 4 6) font (Size 18 24) (Enviroment 0 0 0 0 0) (Size 1440 900) 2 0 [5,5] [3, 3] [3, 3] [[], []] [[], []] [[], []] [True, True] (-1) 0 [] [] 0

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
deckTohand :: StateT World (StateT World Game) ()
deckTohand = do
    now <- use nowplayer
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

--全流れ
fieldclear world = getCanPuts $ (world&field.~[])&initiations.~replicate (world^.maxplayer) True

--次のプレイヤーへ
nextplayer :: World -> World
nextplayer world = world&nowplayer.~((1 + world^.nowplayer) `mod` world^.maxplayer)

--手札からn枚目のカードをドリップ
dripHandcard :: Int -> StateT World (StateT World Game) ModCard
dripHandcard n = do
    now <- use nowplayer
    hcards <- use handcards
    let nowhcard = hcards!!now
        nowcard = nowhcard!!n
    handcards .= (take now hcards) ++ [(take n nowhcard) ++ (drop (n + 1) nowhcard)] ++ (drop (now + 1) hcards)
    return nowcard