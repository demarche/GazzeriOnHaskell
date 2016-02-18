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
--      s   : カードの回転
--      ts  : 追加される木
insertCard :: Int -> ModCard -> Int -> [Tree] -> [Tree]
insertCard x y s ts = inserts x tree ts where
    tree = toTree y (States 0 0 s) ts [x + 2]
inserts :: Int -> Tree -> [Tree] -> [Tree]
inserts x y ts = map (\t -> insert x y t) ts
insert x y (Fork h) = Fork $ h&trees.~map (insert x y) (h^.trees)
insert x y (Passage p) = if x == p then y else Passage p
insert _ _ DeadEnd = DeadEnd

-- 未使用PassageIDを木から探索
unusedPassage :: [Tree] -> [Int]
unusedPassage t = [x | x <-[0..], not (x `elem` usedPassages t)]
usedPassages ts = concat $ map usedPassage ts
usedPassage (Fork h) = concat $ map usedPassage (h^.trees)
usedPassage (Passage p) = [p]
usedPassage DeadEnd = []

-- Card xをTreeに変換
toTree :: ModCard -> States -> [Tree] -> [Int] -> Tree
toTree x s trees ignores = Fork (Hub x s forks2)
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
--      x   : 追加したいカード
--      s   : カードの状態
--      ts  : 追加される木
initiation x s ts = (toTree x s ts []):ts

-- フィールド更新
updateField :: World -> World
updateField world = world&field.~f' where
    f1 = map propCoordinate (world^.field)
    f' = [appDeadend t1 (world^.cardsizeMin) (world^.fieldsize) f1 | t1 <- f1]
-- 座標伝搬
--      tree    : 更新対象の木
--      n       : 親からみた自分の位置
--      state   : 親の状態
--      old_c   : 親のカード
propCoordinate :: Tree -> Tree
propCoordinate tree = case tree of
    (Fork h) -> Fork (h&trees.~(map prop $ zip (h^.trees) $ map (`mod` 4) [4-h^.states^.turn..])) where
        prop (child, n) = case child of
            (Fork hc) -> propCoordinate $ Fork (hc&states.~(connectedState hc h n))
            _ -> child
    _ -> tree
-- おけない場所のDeadEnd化
appDeadend :: Tree -> Size -> Size -> [Tree] -> Tree
appDeadend tree d fsize f = case tree of
    (Fork h) ->  Fork (h&trees.~(map app $ zip (h^.trees) $ map (`mod` 4) [4-h^.states^.turn..])) where
        app (child, n) = case child of
            (Passage p) -> if cannotPutFlag || collisionDetection then DeadEnd else Passage p where
                --フィールドオーバーチェック
                doubleCardTmp = [Hub (ModCard [-1,-1,-1,-1] d 0) (States 0 0 a) []| a <- [0,1]] -- 最小サイズカードの縦横版（座標未計算）
                doubleCard = [a&states.~(connectedState a h n) | a <- doubleCardTmp] -- 上記の座標計算済み
                cannotPutFlag = and [fieldover a d fsize | a <- map (\x -> x^.states) doubleCard]
                --コリジョンチェック
                collisionDetection = and [isCollision a f | a <- doubleCard]
            _ -> appDeadend child d fsize f
    _ -> tree

showcanputs world = forM_ ((world^.handcards)!!(world^.nowplayer)) $ \crd -> forM_ (world^.field) $ \tree -> forM_ [0..length (crd^.connector)-1] $ \trn -> showcanput crd trn tree world
showcanput crd trn tree world = case tree of
    (Fork h) -> forM_ (zip (h^.trees) (map (`mod` 4) [4-h^.states^.turn..])) $ \z -> putlist z where
        putlist (child, n) = case child of
            (Fork hc) -> forM_ (hc^.trees) $ \t -> showcanput crd trn t world
            (Passage p) -> do
                embedIO $ print $ "parent: " ++ (show parentConnector) ++ ", child: " ++ (show childConnector)
                embedIO $ print $ not colDetection
                embedIO $ print $ not cannotPutFlag where
                    parentConnector = (h^.card^.connector)!!((n + h^.states^.turn) `mod` 4)
                    childConnector = (crd^.connector)!!((n + trn + 2) `mod` 4)
                    --衝突判定
                    connected = connectedState (Hub crd (States 0 0 trn) []) h n
                    colDetection = True `elem` [isCollision (Hub crd connected []) (world^.field)]
                    --フィールドオーバーチェック
                    cannotPutFlag = fieldover connected (crd^.size) (world^.fieldsize)
            _ -> embedIO $ print ""
    _ -> embedIO $ print ""

-- worldから置ける場所を取得（イニシエーション以外）
canput :: ModCard -> Int -> Tree -> World -> [(Int, States)]
canput crd trn tree world = case tree of
    (Fork h) -> concat $ map putlist $ zip (h^.trees) $ map (`mod` 4) [4-h^.states^.turn..] where
        putlist (child, n) = case child of
            (Fork hc) -> canput crd trn (Fork hc) world
            (Passage p) -> if (parentConnector < 0 || parentConnector == childConnector) && not colDetection && not cannotPutFlag then [(p, connected)] else [] where
                parentConnector = (h^.card^.connector)!!((n + h^.states^.turn) `mod` 4)
                childConnector = (crd^.connector)!!((n + trn + 2) `mod` 4)
                --衝突判定
                connected = connectedState (Hub crd (States 0 0 trn) []) h n
                colDetection = True `elem` [isCollision (Hub crd connected []) (world^.field)]
                --フィールドオーバーチェック
                cannotPutFlag = fieldover connected (crd^.size) (world^.fieldsize)
            _ -> []
    _ -> []
-- worldから置ける場所を取得（イニシエーションのみ）
canputInit :: ModCard -> Int -> World -> [(Int, States)]
canputInit crd trn world = filter iscol allinitposes where
    turned = normalizeCardSize trn (crd^.size)
    fsize = world^.fieldsize
    allinitposes = case (world^.nowplayer) `mod` 4 of
        0 -> [(-1, States x (fsize^.height - turned^.height) trn) | x <- [0..(fsize^.width - turned^.width)]]
        1 -> [(-1, States x 0 trn) | x <- [0..(fsize^.width - turned^.width)]]
    iscol = \x -> not $ isCollision (Hub crd (snd x) []) (world^.field)

-- 木ごとのバースト数
burstCounter tree = case tree of
    (Fork h) -> fmap (+1) $ fmap sum $ sequence $ map burstCounter (h^.trees)
    (Passage p) -> Nothing
    DeadEnd -> Just 0

-- フィールドオーバー判定
fieldover :: States -> Size -> Size -> Bool
fieldover st csize fsize = st^.posx < 0 || st^.posx + (normalizeCardSize (st^.turn) csize)^.width > fsize^.width || st^.posy < 0 || st^.posy + (normalizeCardSize (st^.turn) csize)^.height > fsize^.height

-- 衝突判定
--      Hub     : 置こうとしているハブ
--      tree    : 探索対象の木
isCollision :: Hub -> [Tree] -> Bool
isCollision h f = True `elem ` [collisionCheck (h^.states) normalized t | t <- f]　where normalized = normalizeCardSize (h^.states^.turn) (h^.card^.size)
collisionCheck :: States -> Size -> Tree -> Bool
collisionCheck (States sx sy st) ss tree = case tree of
    (Fork h) ->if detect then True else True `elem` [collisionCheck (States sx sy st) ss mtree | mtree <- h^.trees] where -- 枝についてコリジョンチェック
            tsize = normalizeCardSize (h^.states^.turn) (h^.card^.size)
            tx = h^.states^.posx; ty = h^.states^.posy
            detect = True `elem` [mx >= tx && mx < tx + tsize^.width && my >= ty && my < ty + tsize^.height | mx<-[sx..sx+ss^.width-1], my<-[sy..sy+ss^.height-1]] -- 現在の節についてコリジョンチェック
    _ -> False

-- カード連結後の座標計算
--      Hub     : 子の情報
--      Hub'    : 親の情報
--      n       : 親からみた自分の位置
connectedState :: Hub -> Hub -> Int -> States
connectedState (Hub c s _) (Hub c' s' _) n = States x' y' (s^.turn) where
    turnedPSize = normalizeCardSize (s'^.turn) (c'^.size)
    turnedCSize = normalizeCardSize (s^.turn) (c^.size)
    x' = case n of
        1 -> (s'^.posx) + turnedPSize^.width
        3 -> (s'^.posx) - turnedCSize^.width
        _ -> (s'^.posx) + ((turnedPSize^.width - turnedCSize^.width) `div` 2)
    y' = case n of
        0 -> (s'^.posy) - turnedCSize^.height
        2 -> (s'^.posy) + turnedPSize^.height
        _ -> (s'^.posy) + ((turnedPSize^.height - turnedCSize^.height) `div` 2)

-- カードサイズの向きによる正規化
normalizeCardSize :: Int -> Size -> Size
normalizeCardSize t s = if even t then s else Size{_width = (s^.height), _height = (s^.width)}