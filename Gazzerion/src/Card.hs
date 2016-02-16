module Card where
import Header
import System.Random
import Control.Monad (replicateM)
import Control.Monad.State
import Control.Lens

-- ランダムなコネクタのカード
randCard = do
    lst <- rands
    return ModCard {_connector = lst, _image = 0, _size = Size 4 6}
    where
        rands = replicateM 4 $ (getStdRandom $ randomR (0,5) :: IO Int)

fixedrand :: IO [Int]
fixedrand = do
    let rands = replicateM 4 $ (getStdRandom $ randomR (-5,5) :: IO Int)
    r <- rands
    let r2 = if null $ filter (> 0) r then fixedrand else rands
    res <- r2
    return res

-- ランダムなコネクタのカード（4が少ない）
randCard2 = do
    r <- fixedrand
    lst <- forM r $ \x -> if x < 0 then return 0 else return x
    return ModCard {_connector = lst, _image = 0, _size = Size 4 6}