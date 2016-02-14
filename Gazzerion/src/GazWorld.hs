{-# LANGUAGE TemplateHaskell #-}
module GazWorld where
import Card
import Field
import FreeGame
import Control.Lens
import Control.Monad.State

v2Int x y = V2 ((fromIntegral x) :: Double) ((fromIntegral y) :: Double)

data Mode = Init | Choice | Move Int | Scroll Double | Dist Double

data Enviroment = Enviroment
    { _grid :: Int
    , _gridx :: Int
    , _gridy :: Int
    , _gridw :: Int
    , _gridh :: Int}
data World = World
    {_mode :: Mode
    , _cardsizeMax :: Size
    , _font :: Font
    , _fieldsize :: Size
    , _enviroment :: Enviroment
    , _screensize :: Size
    , _maxplayer :: Int
    , _nowplayer :: Int
    , _deckmax :: [Int]
    , _maxhandcards :: [Int]
    , _maxmagiccards :: [Int]
    , _decks :: [[ModCard]]
    , _handcards :: [[ModCard]]
    , _magics :: [[ModCard]]
    , _initiations :: [Bool]
    }
makeLenses ''World
makeLenses '' Enviroment

makeWorld font = World Init (Size 4 6) font (Size 18 24) (Enviroment 0 0 0 0 0) (Size 640 480) 2 0 [15, 15] [3, 3] [3, 3] [[], []] [[], []] [[], []] [True, True]

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
    let getDeck n = replicateM n $ embedIO $ randCard
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