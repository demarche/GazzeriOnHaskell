module Card where
import System.Random
import Control.Monad (replicateM)

-- カード
-- connector = コネクタ
-- （将来的に）画像
data ModCard = ModCard{ connector :: [Int]} deriving Show

-- ランダムなコネクタのカード
randCard = do
    lst <- rands
    return ModCard{ connector= lst }
    where
        rands = replicateM 4 $ (getStdRandom $ randomR (0,5) :: IO Int)

