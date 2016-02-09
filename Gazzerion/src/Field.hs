module Field  where
import Card

fieldSize = Size 18 24

-- カードの状態 x y turn
data State = State{posx :: Int, posy :: Int, turn :: Int} deriving Show

-- ギャザリオンの木
-- DeadEnd = 接続不可能な枝
-- Passage = 接続可能な枝
--      Int     : 枝の識別番号（葉の追加用）　4を法とすると、0は常に上を向く
-- Fork = 節（カード）
--      ModCard : カード
--      State   : 座標・回転
--      [Tree]  : 次の木
data Tree = DeadEnd | Passage Int | Fork {card :: ModCard, state :: State, trees :: [Tree]} deriving Show

-- 木にCard yを追加
--      x   : 追加される枝のPassage ID
--      y   : 追加したいカード
--      s   : カードの状態
--      t   : 追加される木
insertCard :: Int -> ModCard -> Int -> Tree -> Tree
insertCard x y s t = insert x tree t where
    tree = toTree y s t [x + 2]
insert :: Int -> Tree -> Tree -> Tree
insert x y (Fork c s t) = Fork c s $ map (insert x y) t
insert x y (Passage p) = if x == p then y
                                   else Passage p
insert _ _ DeadEnd = DeadEnd

-- 未使用PassageIDを木から探索
unusedPassage t = [x | x <-[0..], not(x `elem` usedPassage t)]
usedPassage (Fork _ _ t) = foldl (++) [] $ map usedPassage t
usedPassage (Passage p) = [p]
usedPassage (DeadEnd) = []

-- Card xをTreeに変換
toTree :: ModCard -> Int -> Tree -> [Int] -> Tree
toTree x s tree ignores = Fork x (State 0 0 s) [if (y `mod` len) `elem` map (`mod` len) ignores then DeadEnd else Passage y | y <-ids]
    where
        -- PassageID割り当て
        un = (unusedPassage tree)
        len = length $ connector x -- コネクタの数
        moduloID z = filter (\y -> y `mod` len==(z+len-s) `mod` len) un -- コネクタの数を法とするxと等しい未使用パッセージの無限リスト
        ids = [(moduloID x)!!0| x<-[0..len-1]]

-- イニシエーション
initiation x s = toTree x s (DeadEnd) []

-- tree更新
--      tree    : 更新する木
--      [tree]  : フィールドの木全体
--      size    : 最小のカードサイズ（このカードが置けなかったらDeadEnd）
refreshTree :: Tree -> [Tree] -> Size -> Tree
refreshTree t forest defaultSize = t' where
    t1 = refreshCoordinate (t, 4) (State 0 0 0) (card t)
    t' = refreshPassage (t1, 4) (State 0 0 0) (card t1) forest defaultSize
-- 座標伝搬
--      tree    : 更新対象の木
--      n       : 親からみた自分の位置
--      state   : 親の状態
--      old_c   : 親のカード
refreshCoordinate :: (Tree, Int) -> State -> ModCard -> Tree
refreshCoordinate ((Fork c s trees), n) (State x y t) old_c = Fork c s' $ map (\z->refreshCoordinate z s' c) $ zip trees $ map (`mod` 4) [4-turn s..]
    where s' = connectedState (Fork c s trees) n (State x y t) old_c
refreshCoordinate ((Passage p), n) (State x y t) c' = Passage p
refreshCoordinate (DeadEnd, _) _ _ = DeadEnd
-- おけない場所のDeadEnd化
refreshPassage ((Fork c s trees), _) _ _ f d = Fork c s $ map (\z->refreshPassage z s c f d) $ zip trees $ map (`mod` 4) [4-turn s..]
refreshPassage ((Passage p), n) (State x y t) c f d = if cannotPutFlag then DeadEnd
        else if collisionDetection then DeadEnd
         else Passage p
    where
     --フィールドオーバーチェック
     doubleCard = [Fork (ModCard [-1,-1,-1,-1] d 0) (State 0 0 a) []| a <- [0,1]] -- 最小サイズカードの縦横版
     doubleCardPos = [connectedState a n (State x y t) c| a <- doubleCard] -- doubleCard連結後の座標
     cannotPutFlag = foldl (&&) True [(posx a) < 0 || (posx a) + width (size c) > (width fieldSize) || (posy a) < 0 || (posy a) + height (size c) > (height fieldSize) | a <- doubleCardPos]

     --コリジョンチェック
     collision (State sx sy st) ss (Fork tc (State tx ty tt) ttree) =
        if detect then True -- 現在の節についてコリジョンチェック
            else True `elem` [collision (State sx sy st) ss mtree | mtree <- ttree] where -- 枝についてコリジョンチェック
                tsize = normalizeCardSize tt (size tc)
                detect = True `elem` [mx >= tx && mx < tx + width tsize && my >= ty && my < ty + height tsize | mx<-[sx..sx+width ss], my<-[sy..sy+height ss]]
     collision _ _ _ = False

     collisionDetection = True `elem` [foldl (&&) True [collision (doubleCardPos!!a) (normalizeCardSize a d) mytree| a <- [0, 1]] | mytree <- f]

refreshPassage (DeadEnd, _) _ _ _ _ = DeadEnd


-- カード連結後の座標計算
--      fork    : 連結する側の情報
--      n       : 親からみた自分の位置
--      state   : 親の状態
--      old_c   : 親のカード
connectedState (Fork c s _) n (State x y t) c' = State x' y' (turn s) where
    turnedPSize = normalizeCardSize t (size c')
    turnedCSize = normalizeCardSize (turn s) (size c)
    x' = case n of
        1 -> x + width turnedPSize
        3 -> x - width turnedCSize
        _ -> x + ((width turnedPSize - width turnedCSize) `div` 2)
    y' = case n of
        0 -> y - height turnedCSize
        2 -> y + height turnedPSize
        _ -> y + ((height turnedPSize - height turnedCSize) `div` 2)

-- カードサイズの向きによる正規化
normalizeCardSize t s = if t `mod` 2 == 0 then s else Size{width = height(s), height = width(s)}