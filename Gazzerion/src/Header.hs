{-# LANGUAGE TemplateHaskell #-}
module Header where
import FreeGame
import Control.Lens

data Size = Size{_width:: Int, _height:: Int} deriving Show

-- カード
-- connector = コネクタ
-- size = カードのサイズ
-- （将来的に）画像
data ModCard = ModCard{ _connector :: [Int], _size :: Size, _image :: Int} deriving Show

data Mode = Init | Draw | Choice | Move Bool | Burst | GameOver | Goto Mode

data Enviroment = Enviroment
    { _grid :: Int
    , _gridx :: Int
    , _gridy :: Int
    , _gridw :: Int
    , _gridh :: Int}
data World = World
    {_mode :: Mode
    , _cardsizeMax :: Size
    , _cardsizeMin :: Size
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
    , _selectedhandcard :: Int
    , _nowturn :: Int
    , _canputs :: [[(Int, States)]]
    , _field :: [Tree]
    , _checkmatedcounter :: Int
    , _lowburst :: Int
    }

-- カードの状態 x y turn
data States = States{_posx :: Int, _posy :: Int, _turn :: Int} deriving (Eq, Show)

-- 接続カード
data Hub = Hub{ _card :: ModCard, _states :: States, _trees :: [Tree]} deriving Show

-- ギャザリオンの木
-- DeadEnd = 接続不可能な枝
-- Passage = 接続可能な枝
--      Int     : 枝の識別番号（葉の追加用）　4を法とすると、0は常に上を向く
-- Fork = 節（カード）
--      ModCard : カード
--      State   : 座標・回転
--      [Tree]  : 次の木
data Tree = DeadEnd | Passage Int | Fork Hub deriving Show

makeLenses ''Hub
makeLenses ''States
makeLenses ''World
makeLenses '' Enviroment
makeLenses ''ModCard
makeLenses ''Size

