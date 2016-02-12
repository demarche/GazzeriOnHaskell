{-# LANGUAGE TemplateHaskell #-}
module Card where
import System.Random
import Control.Monad (replicateM)
import Control.Lens

data Size = Size{_width:: Int, _height:: Int} deriving Show

-- カード
-- connector = コネクタ
-- size = カードのサイズ
-- （将来的に）画像
data ModCard = ModCard{ _connector :: [Int], _size :: Size, _image :: Int} deriving Show
makeLenses ''ModCard
makeLenses ''Size

-- ランダムなコネクタのカード
randCard = do
    lst <- rands
    return ModCard {_connector = lst, _image = 0, _size = Size 4 6}
    where
        rands = replicateM 4 $ (getStdRandom $ randomR (0,5) :: IO Int)
