module Field  where
import Card

-- カードの状態 x y turn
data State = State{x :: Int, y :: Int, turn :: Int} deriving Show

-- ギャザリオンの木
-- DeadEnd = 接続不可能な枝
-- Passage = 接続可能な枝
--      Int     : 枝の識別番号（葉の追加用）　4を法とすると、0は常に上を向く
-- Fork = 節（カード）
--      ModCard : カード
--      State   : 座標・回転
--      [Tree]  : 次の木
data Tree =  DeadEnd | Passage Int | Fork ModCard State [Tree] deriving Show

-- 木にCard yを追加
--      x   : 追加される枝のPassage ID
--      y   : 追加したいカード
--      s   : カードの状態
--      t   : 追加される木
insertCard :: Int -> ModCard -> State -> Tree -> Tree
insertCard x y s t = insert x tree t where
    tree = toTree y s t [x + 2]
insert x y (Fork card state t) = Fork card state $ map (insert x y) t
insert x y (Passage p) = if x == p then y
                                   else Passage p
insert x y DeadEnd = DeadEnd

-- 未使用PassageIDを木から探索
unusedPassage t = [x | x <-[0..], not(x `elem` usedPassage t)]
usedPassage (Fork _ _ t) = foldl (++) [] $ map usedPassage t
usedPassage (Passage p) = [p]
usedPassage (DeadEnd) = []

-- Card xをTreeに変換
toTree :: ModCard -> State -> Tree -> [Int] -> Tree
toTree x state tree ignores = Fork x state [if (x `mod` len) `elem` map (`mod` len) ignores
                                            then DeadEnd
                                            else Passage x | x <-ids]
    where
        -- PassageID割り当て
        un = (unusedPassage tree)
        len = length $ connector x -- コネクタの数
        moduloID x = filter (\y -> y `mod` len==(x+len-turn state) `mod` len) un -- コネクタの数を法とするxと等しい未使用パッセージの無限リスト
        ids = [(moduloID x)!!0| x<-[0..len-1]]

-- イニシエーション
initiation x state = toTree x state (DeadEnd) []


