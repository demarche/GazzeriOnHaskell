module Field  where
import Card
import Header
import Control.Lens
import Control.Monad.State
import FreeGame

fieldSize = Size 18 24

-- 木にCard yを追加
--      x   : 追加される枝のPassage ID
--      y   : 追加したいカード
--      s   : カードの状態
--      t   : 追加される木
insertCard :: Int -> ModCard -> Int -> [Tree] -> [Tree]
insertCard x y s ts = inserts x tree ts where
    tree = toTree y (States 0 0 s) ts [x + 2]
inserts :: Int -> Tree -> [Tree] -> [Tree]
inserts x y ts = map (\t -> insert x y t) ts
insert x y (Fork c s t) = Fork c s $ map (insert x y) t
insert x y (Passage p) = if x == p then y else Passage p
insert _ _ DeadEnd = DeadEnd

-- 未使用PassageIDを木から探索
unusedPassage :: [Tree] -> [Int]
unusedPassage t = [x | x <-[0..], not (x `elem` usedPassages t)]
usedPassages ts = foldl (++) [] $ map usedPassage ts
usedPassage (Fork _ _ t) = foldl (++) [] $ map usedPassage t
usedPassage (Passage p) = [p]
usedPassage DeadEnd = []

-- Card xをTreeに変換
toTree :: ModCard -> States -> [Tree] -> [Int] -> Tree
toTree x s trees ignores = Fork x s forks2
    where
        -- PassageID割り当て
        un = (unusedPassage trees)
        len = length $ x^.connector -- コネクタの数
        moduloID z = filter (\y -> y `mod` len==(z+len-(s^.turn)) `mod` len) un -- コネクタの数を法とするxと等しい未使用パッセージの無限リスト
        ids = [(moduloID y)!!0 | y<-[0..len-1]]
        forks = [if (y `mod` len) `elem` map (`mod` len) ignores then DeadEnd else Passage y | y <-ids]
        --connector:0 をDeadEnd化
        forks2 = [if (x^.connector)!!y == 0 then DeadEnd else forks!!y | y <- [0..len-1]]

-- イニシエーション
initiation x s ts = (toTree x s ts []):ts

-- tree更新
--      tree    : 更新する木
--      [tree]  : フィールドの木全体
--      size    : 最小のカードサイズ（このカードが置けなかったらDeadEnd）
refreshTree :: [Tree] -> Size -> [Tree]
refreshTree forest defaultSize = f' where
    f1 = [refreshCoordinate (t, 4) (States (-100) 0 0) (_card t) | t <- forest]
    f' = [refreshPassage (t1, 4) (States 0 0 0) (_card t1) f1 defaultSize | t1 <- f1]
-- 座標伝搬
--      tree    : 更新対象の木
--      n       : 親からみた自分の位置
--      state   : 親の状態
--      old_c   : 親のカード
refreshCoordinate :: (Tree, Int) -> States -> ModCard -> Tree
refreshCoordinate ((Fork c s trees), n) (States x y t) old_c = Fork c s' $ map (\z->refreshCoordinate z s' c) $ zip trees $ map (`mod` 4) [4-s^.turn..]
    where s' = if x /= -100 then connectedState (Fork c s trees) n (States x y t) old_c else s
refreshCoordinate ((Passage p), _) _ _ = Passage p
refreshCoordinate (DeadEnd, _) _ _ = DeadEnd
-- おけない場所のDeadEnd化
refreshPassage :: (Tree, Int) -> States -> ModCard -> [Tree] -> Size -> Tree
refreshPassage ((Fork c s trees), _) _ _ f d = Fork c s $ map (\z->refreshPassage z s c f d) $ zip trees $ map (`mod` 4) [4-s^.turn..]
refreshPassage ((Passage p), n) (States x y t) c f d = if cannotPutFlag || collisionDetection then DeadEnd
    else Passage p
    where
        --フィールドオーバーチェック
        doubleCard = [Fork (ModCard [-1,-1,-1,-1] d 0) (States 0 0 a) []| a <- [0,1]] -- 最小サイズカードの縦横版
        doubleCardPos = [connectedState a n (States x y t) c | a <- doubleCard] -- doubleCard連結後の座標
        cannotPutFlag = and [a^.posx < 0 || a^.posx + (normalizeCardSize (a^.turn) d)^.width > fieldSize^.width ||
            a^.posy < 0 || a^.posy + (normalizeCardSize (a^.turn) d)^.height > fieldSize^.height | a <- doubleCardPos]
        --コリジョンチェック
        collisionDetection = True `elem` [and [collision (doubleCardPos!!a) (normalizeCardSize a d) mytree| a <- [0, 1]] | mytree <- f]
refreshPassage (DeadEnd, _) _ _ _ _ = DeadEnd

refreshTree2 t forest defaultSize = do
    let t1 = refreshCoordinate (t, 4) (States (-100) 0 0) (_card t)
    refreshCoordinate2 (t1, 4) (States (-100) 0 0) (_card t1)
    refreshPassage2 (t1, 4) (States 0 0 0) (_card t1) forest defaultSize
refreshCoordinate2 ((Fork c s trees), n) st old_c = do
    embedIO $ print $ "Card" ++ (show s')
    forM_ (zip trees (map (`mod` 4) [4-s^.turn..])) $ (\z->refreshCoordinate2 z s' c)
    where s' = if (st^.posx) /= -100 then connectedState (Fork c s trees) n st old_c else s
refreshCoordinate2 ((Passage p), _) _ _ = embedIO $ print $ "Passage : " ++ (show p)
refreshCoordinate2 (DeadEnd, _) _ _ = embedIO $ print $ "DeadEnd"
refreshPassage2 ((Fork c s trees), _) _ _ f d = do
    embedIO $ print $ "Card"
    embedIO $ print $ s
    forM_ (zip trees (map (`mod` 4) [4-s^.turn..])) $ (\z->refreshPassage2 z s c f d)
refreshPassage2 ((Passage p), n) (States x y t) c f d = do
    embedIO $ print $ "Passage : " ++ (show p) ++ ", n : " ++ (show n)
    embedIO $ print $ doubleCardPos
    embedIO $ print $ cannotPutFlag
    embedIO $ print $ collisionDetection
    embedIO $ print $ [collision (doubleCardPos!!a) (normalizeCardSize a d) (f!!0)| a <- [0, 1]]
    where
     --フィールドオーバーチェック
     --abc = if p `mod` 4 == n then False else True
     doubleCard = [Fork (ModCard [-1,-1,-1,-1] d 0) (States 0 0 a) []| a <- [0,1]] -- 最小サイズカードの縦横版
     doubleCardPos = [connectedState a n (States x y t) c | a <- doubleCard] -- doubleCard連結後の座標
     cannotPutFlag = and [a^.posx < 0 || a^.posx + (normalizeCardSize (a^.turn) d)^.width > fieldSize^.width ||
        a^.posy < 0 || a^.posy + (normalizeCardSize (a^.turn) d)^.height > fieldSize^.height | a <- doubleCardPos]
     --コリジョンチェック
     collisionDetection = True `elem` [and [collision (doubleCardPos!!a) (normalizeCardSize a d) mytree| a <- [0, 1]] | mytree <- f]
refreshPassage2 (DeadEnd, _) _ _ _ _ = embedIO $ print "dead"

-- おける場所のPassage ID
getCanPuts :: StateT World (StateT World (StateT World Game)) ()
getCanPuts = do
    now <- use nowplayer
    hcards <- use handcards
    shc <- use selectedhandcard
    fld <- use field
    init <- use initiations
    world <- get
    let normalputs = \crd -> concat $ [getCanPutNoInit crd trn (mytree, 0) DeadEnd fld | mytree <- (world^.field), trn <- [0..length (crd^.connector)-1]] -- イニシエーション以外のおける場所
        initputs = \crd -> if init!!now
            then concat $ [getCanPutInit crd trn world | trn <- [0..length (crd^.connector)-1]]
            else []
    canputs .= [initputs crd ++ normalputs crd | crd <- (hcards!!now)]
getCanPutNoInit :: ModCard -> Int -> (Tree, Int) -> Tree -> [Tree] -> [(Int, States)]
getCanPutNoInit crd trn ((Fork c s t), _) _ f = foldl (++) [] $ map (\x -> getCanPutNoInit crd trn x (Fork c s t) f) $ zip t $ map (`mod` 4) [4-s^.turn..]
getCanPutNoInit crd trn ((Passage p), n) (Fork c s t) f = if (parentCnctor < 0 || parentCnctor == (crd^.connector)!!((n + trn + 2) `mod` 4)) && not collisionDetection
    then [(p, connected)]
    else [] where
        parentCnctor = (c^.connector)!!((n + s^.turn) `mod` 4)
        tempCard = Fork crd (States 0 0 trn) []
        connected = connectedState tempCard n s c
        collisionDetection = True `elem` [collision connected (normalizeCardSize trn (crd^.size)) mytree | mytree <- f]
getCanPutNoInit _ _ ((DeadEnd), _) _ _ = []
getCanPutInit :: ModCard -> Int -> World -> [(Int, States)]
getCanPutInit crd trn world = filter iscol allinitposes where
    turned = normalizeCardSize trn (crd^.size)
    fsize = world^.fieldsize
    allinitposes = case (world^.nowplayer) `mod` 4 of
        0 -> [(-1, States x (fsize^.height - turned^.height) trn) | x <- [0..(fsize^.width - turned^.width)]]
    iscol = \x -> not $  True `elem` [collision (snd x) turned mytree | mytree <- (world^.field)]

-- 衝突判定
--      state   : 置こうとしているカードの位置
--      size    : 置こうとしているカードの正規化済みサイズ
--      tree    : 探索対象の木
collision :: States -> Size -> Tree -> Bool
collision (States sx sy st) ss (Fork tc (States tx ty tt) ttree) =
    if detect -- 現在の節についてコリジョンチェック
        then True
        else True `elem` [collision (States sx sy st) ss mtree | mtree <- ttree] where -- 枝についてコリジョンチェック
            tsize = normalizeCardSize tt (tc^.size)
            detect = True `elem` [mx >= tx && mx < tx + tsize^.width && my >= ty && my < ty + tsize^.height | mx<-[sx..sx+ss^.width-1], my<-[sy..sy+ss^.height-1]]
collision _ _ _ = False

-- カード連結後の座標計算
--      fork    : 連結する側の情報
--      n       : 親からみた自分の位置
--      state   : 親の状態
--      old_c   : 親のカード
connectedState :: Tree -> Int -> States -> ModCard -> States
connectedState (Fork c s _) n (States x y t) c' = States x' y' (s^.turn) where
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