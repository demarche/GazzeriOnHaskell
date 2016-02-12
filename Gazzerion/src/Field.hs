{-# LANGUAGE TemplateHaskell #-}
module Field  where
import Card
import Control.Lens

fieldSize = Size 18 24

-- カードの状態 x y turn
data State = State{_posx :: Int, _posy :: Int, _turn :: Int} deriving Show

-- ギャザリオンの木
-- DeadEnd = 接続不可能な枝
-- Passage = 接続可能な枝
--      Int     : 枝の識別番号（葉の追加用）　4を法とすると、0は常に上を向く
-- Fork = 節（カード）
--      ModCard : カード
--      State   : 座標・回転
--      [Tree]  : 次の木
data Tree = DeadEnd | Passage Int | Fork {_card :: ModCard, _state :: State, _trees :: [Tree]} deriving Show

makeLenses ''Tree
makeLenses ''State

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
insert x y (Passage p) = if x == p then y else Passage p
insert _ _ DeadEnd = DeadEnd

-- 未使用PassageIDを木から探索
unusedPassage t = [x | x <-[0..], not(x `elem` usedPassage t)]
usedPassage (Fork _ _ t) = foldl (++) [] $ map usedPassage t
usedPassage (Passage p) = [p]
usedPassage DeadEnd = []

-- Card xをTreeに変換
toTree :: ModCard -> Int -> Tree -> [Int] -> Tree
toTree x s tree ignores = Fork x (State 0 0 s) forks2
    where
        -- PassageID割り当て
        un = (unusedPassage tree)
        len = length $ x^.connector -- コネクタの数
        moduloID z = filter (\y -> y `mod` len==(z+len-s) `mod` len) un -- コネクタの数を法とするxと等しい未使用パッセージの無限リスト
        ids = [(moduloID y)!!0 | y<-[0..len-1]]
        forks = [if (y `mod` len) `elem` map (`mod` len) ignores then DeadEnd else Passage y | y <-ids]
        --connector:0 をDeadEnd化
        cantConnect = [if (x^.connector)!!((y+s) `mod` len)==0 then True else False | y <- [0..len-1]]
        forks2 = [if snd y then DeadEnd else fst y | y <- zip forks cantConnect]


-- イニシエーション
initiation x s = toTree x s (DeadEnd) []

-- tree更新
--      tree    : 更新する木
--      [tree]  : フィールドの木全体
--      size    : 最小のカードサイズ（このカードが置けなかったらDeadEnd）
refreshTree :: Tree -> [Tree] -> Size -> Tree
refreshTree t forest defaultSize = t' where
    t1 = refreshCoordinate (t, 4) (State 0 0 0) (_card t)
    t' = refreshPassage (t1, 4) (State 0 0 0) (_card t1) forest defaultSize
-- 座標伝搬
--      tree    : 更新対象の木
--      n       : 親からみた自分の位置
--      state   : 親の状態
--      old_c   : 親のカード
refreshCoordinate :: (Tree, Int) -> State -> ModCard -> Tree
refreshCoordinate ((Fork c s trees), n) (State x y t) old_c = Fork c s' $ map (\z->refreshCoordinate z s' c) $ zip trees $ map (`mod` 4) [4-s^.turn..]
    where s' = connectedState (Fork c s trees) n (State x y t) old_c
refreshCoordinate ((Passage p), _) _ _ = Passage p
refreshCoordinate (DeadEnd, _) _ _ = DeadEnd
-- おけない場所のDeadEnd化
refreshPassage :: (Tree, Int) -> State -> ModCard -> [Tree] -> Size -> Tree
refreshPassage ((Fork c s trees), _) _ _ f d = Fork c s $ map (\z->refreshPassage z s c f d) $ zip trees $ map (`mod` 4) [4-s^.turn..]
refreshPassage ((Passage p), n) (State x y t) c f d = if cannotPutFlag then DeadEnd
        else if collisionDetection then DeadEnd
         else Passage p
    where
     --フィールドオーバーチェック
     doubleCard = [Fork (ModCard [-1,-1,-1,-1] d 0) (State 0 0 a) []| a <- [0,1]] -- 最小サイズカードの縦横版
     doubleCardPos = [connectedState a n (State x y t) c | a <- doubleCard] -- doubleCard連結後の座標
     cannotPutFlag = foldl (&&) True [a^.posx < 0 || a^.posx + c^.size^.width > fieldSize^.width || a^.posy < 0 || a^.posy + c^.size^.height > fieldSize^.height | a <- doubleCardPos]
     --コリジョンチェック
     collisionDetection = True `elem` [foldl (&&) True [collision (doubleCardPos!!a) (normalizeCardSize a d) mytree| a <- [0, 1]] | mytree <- f]
refreshPassage (DeadEnd, _) _ _ _ _ = DeadEnd

-- おける場所のPassage ID
getCanPutPassageIDs :: ModCard -> Int -> [Tree] -> [[Int]]
getCanPutPassageIDs crd trn ts =[getCanPutPassageIDsByTree crd trn (mytree, 0) DeadEnd ts | mytree <- ts]
getCanPutPassageIDsByTree :: ModCard -> Int -> (Tree, Int) -> Tree -> [Tree] -> [Int]
getCanPutPassageIDsByTree crd trn ((Fork c s t), _) _ f = foldl (++) [] $ map (\x -> getCanPutPassageIDsByTree crd trn x (Fork c s t) f) $ zip t $ map (`mod` 4) [4-s^.turn..]
getCanPutPassageIDsByTree crd trn ((Passage p), n) (Fork c s t) f = if (parentCnctor < 0 || parentCnctor == (crd^.connector)!!((n + trn + 2) `mod` 4)) && not collisionDetection
    then [p]
    else [] where
        parentCnctor = (c^.connector)!!((n + s^.turn) `mod` 4)
        tempCard = Fork crd (State 0 0 trn) []
        connected = connectedState tempCard n s c
        collisionDetection = True `elem` [collision connected (normalizeCardSize trn (crd^.size)) mytree | mytree <- f]
getCanPutPassageIDsByTree _ _ ((DeadEnd), _) _ _ = []

-- 衝突判定
--      state   : 置こうとしているカードの位置
--      size    : 置こうとしているカードの正規化済みサイズ
--      tree    : 探索対象の木
collision :: State -> Size -> Tree -> Bool
collision (State sx sy st) ss (Fork tc (State tx ty tt) ttree) =
    if detect -- 現在の節についてコリジョンチェック
        then True
        else True `elem` [collision (State sx sy st) ss mtree | mtree <- ttree] where -- 枝についてコリジョンチェック
            tsize = normalizeCardSize tt (tc^.size)
            detect = True `elem` [mx >= tx && mx < tx + tsize^.width && my >= ty && my < ty + tsize^.height | mx<-[sx..sx+ss^.width], my<-[sy..sy+ss^.height]]
collision _ _ _ = False

-- カード連結後の座標計算
--      fork    : 連結する側の情報
--      n       : 親からみた自分の位置
--      state   : 親の状態
--      old_c   : 親のカード
connectedState :: Tree -> Int -> State -> ModCard -> State
connectedState (Fork c s _) n (State x y t) c' = State x' y' (s^.turn) where
    turnedPSize = normalizeCardSize t (c'^.size)
    turnedCSize = normalizeCardSize (s^.turn) (c^.size)
    x' = case n of
        1 -> x + turnedPSize^.width
        3 -> x - turnedCSize^.width
        _ -> x + ((turnedPSize^.width - turnedCSize^.width) `div` 2)
    y' = case n of
        0 -> y - turnedCSize^.height
        2 -> y + turnedPSize^.height
        _ -> y + ((turnedPSize^.height - turnedCSize^.height) `div` 2)

-- カードサイズの向きによる正規化
normalizeCardSize :: Int -> Size -> Size
normalizeCardSize t s = if even t then s else Size{_width = (s^.height), _height = (s^.width)}