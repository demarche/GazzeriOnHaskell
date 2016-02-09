module Card where
import System.Random
import Control.Monad (replicateM)

data Size = Size{width:: Int, height:: Int} deriving Show

-- カード
-- connector = コネクタ
-- size = カードのサイズ
-- （将来的に）画像
data ModCard = ModCard{ connector :: [Int], size :: Size, image :: Int} deriving Show

-- ランダムなコネクタのカード
randCard = do
    lst <- rands
    return ModCard {connector = lst, image = 0, size = Size 4 6}
    where
        rands = replicateM 4 $ (getStdRandom $ randomR (0,5) :: IO Int)
