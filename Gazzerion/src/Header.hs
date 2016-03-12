{-# LANGUAGE TemplateHaskell #-}
module Header where
import FreeGame
import Control.Lens

data Size = Size{_width:: Int, _height:: Int} deriving (Eq, Show)

-- カード
-- connector = コネクタ
-- size = カードのサイズ
-- （将来的に）画像
data ModCard = ModCard{ _connector :: [Int], _size :: Size, _image :: Int} deriving (Eq, Show)

data MagicPhase = Interpreter [Int] | SelectFieldCard Int [Int] | SelectField [Int] ModCard Bool | End deriving Show

data Mode = Init | Draw | MateCheck | Choice | Magic [String] MagicPhase | Move Bool | Mate Int String | Burst Int [Tree] Mode | GameOver | Goto Mode | PError String deriving Show

data Enviroment = Enviroment
    { _grid :: Int
    , _gridx :: Int
    , _gridy :: Int
    , _gridw :: Int
    , _gridh :: Int
    , _basestart :: [(V2 Double, V2 Double)]}
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
    , _magiccards :: [[MagicCard]]
    , _initiations :: [Maybe Int]
    , _selectedhandcard :: Int
    , _selectedmagiccard :: Int
    , _nowturn :: Int
    , _canputs :: [[(Int, States)]]
    , _canmagic :: [[Bool]]
    , _field :: [Tree]
    , _checkmatedcounter :: Int
    , _lowburst :: Int
    , _costs :: [Int]
    , _numofput :: Int
    }

-- カードの状態 x y turn
data States = States{_posx :: Int, _posy :: Int, _turn :: Int} deriving (Eq, Show)

-- 接続カード
data Hub = Hub{ _card :: ModCard, _states :: States, _trees :: [Tree]} deriving (Eq, Show)

-- ギャザリオンの木
-- DeadEnd = 接続不可能な枝
-- Passage = 接続可能な枝
--      Int     : 枝の識別番号（葉の追加用）　4を法とすると、0は常に上を向く
-- Fork = 節（カード）
--      ModCard : カード
--      State   : 座標・回転
--      [Tree]  : 次の木
data Tree = DeadEnd | NotConnect | Passage Int | Fork Hub deriving (Eq, Show)

data DeckCost = S | A | B | C | D | E deriving Show
data MagicCard = MagicCard{_name :: String, _cost :: Int, _deckcost :: DeckCost, _desc :: String, _funcs :: [String], _img :: Bitmap}

makeLenses ''Hub
makeLenses ''States
makeLenses ''World
makeLenses '' Enviroment
makeLenses ''ModCard
makeLenses ''Size
makeLenses ''MagicCard

