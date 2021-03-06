module Field  where
import Card
import Header
import Control.Lens
import Control.Monad.State
import FreeGame
import Data.Bits
import Data.List(delete)

fieldSize = Size 18 24

-- 木にCard yを追加
--      x   : 追加される枝のPassage ID
--      y   : 追加したいカード
--      s   : カードの回転
--      ts  : 追加される木
insertCard :: Int -> ModCard -> Int -> [Tree] -> [Tree]
insertCard x y s ts = inserts x tree ts where
    tree = toTree y (States (-1) (-1) s) ts [x + 2]
inserts :: Int -> Tree -> [Tree] -> [Tree]
inserts x y ts = map (\t -> insert x y t) ts
insert x y (Fork h) = Fork $ h&trees.~map (insert x y) (h^.trees)
insert x y (Passage p) = if x == p then y else Passage p
insert _ _ t = t

-- 未使用PassageIDを木から探索
unusedPassage :: [Tree] -> [Int]
unusedPassage t = [x | x <-[0..], not (x `elem` usedPassages t)]
usedPassages ts = concatMap usedPassage ts
usedPassage (Fork h) = concatMap usedPassage (h^.trees)
usedPassage (Passage p) = [p]
usedPassage _ = []

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
        --connector:0 をNotConnect化
        forks2 = [if (x^.connector)!!y == 0 then NotConnect else forks!!y | y <- [0..len-1]]

-- イニシエーション
--      x   : 追加したいカード
--      s   : カードの状態
--      ts  : 追加される木
initiation x s ts = (toTree x s ts []):ts

-- 座標伝搬
--      tree    : 更新対象の木
--      n       : 親からみた自分の位置
--      state   : 親の状態
--      old_c   : 親のカード
propCoordinate :: Tree -> Tree
propCoordinate tree = case tree of
    (Fork h) -> Fork (h&trees.~(zipWith prop (h^.trees) $ map (`mod` 4) [4-h^.states^.turn..])) where
        prop child n = case child of
            (Fork hc) -> propCoordinate $ Fork (hc&states.~if hc^.states^.posx < 0 || hc^.states^.posy < 0 then connectedState hc h n else hc^.states)
            _ -> child
    _ -> tree
-- おけない場所のDeadEnd化
appDeadend :: Tree -> Size -> Size -> [Tree] -> Tree
appDeadend tree d fsize f = case tree of
    (Fork h) ->  Fork (h&trees.~(zipWith app (h^.trees) $ map (`mod` 4) [4-h^.states^.turn..])) where
        app child n = case child of
            (Passage p) -> if cannotPutFlag then DeadEnd else Passage p where
                doubleCardTmp = [Hub (ModCard [-1,-1,-1,-1] d 0) (States 0 0 a) []| a <- [0,1]] -- 最小サイズカードの縦横版（座標未計算）
                doubleCard = [a&states.~(connectedState a h n) | a <- doubleCardTmp] -- 上記の座標計算済み
                --フィールドオーバー＆コリジョンチェック
                cannotPutFlag = all (\a -> isCollision a f || fieldover (a^.states) d fsize) doubleCard
            _ -> appDeadend child d fsize f
    _ -> tree

-- worldから手札の置ける場所リストを取得（イニシエーション以外）
canput :: ModCard -> Int -> Tree -> World -> [(Int, States)]
canput crd trn tree world =
    case tree of
        (Fork h) -> concat $ zipWith putlist (h^.trees) $ map (`mod` 4) [4-h^.states^.turn..] where
            putlist child n = case child of
                (Fork hc) -> canput crd trn (Fork hc) world
                (Passage p) -> if connect && not colDetection && not cannotPutFlag then [(p, connected)] else [] where
                    connect = isConnect h (Hub crd (States 0 0 trn) []) n
                    --衝突判定
                    connected = connectedState (Hub crd (States 0 0 trn) []) h n
                    colDetection = isCollision (Hub crd connected []) (world^.field)
                    --フィールドオーバーチェック
                    cannotPutFlag = fieldover connected (crd^.size) (world^.fieldsize)
                _ -> []
        _ -> []

-- worldから手札の置ける場所リストを取得（イニシエーションのみ）
canputInit :: ModCard -> Int -> World -> [(Int, States)]
canputInit crd trn world =
    let turned = normalizeCardSize trn (crd^.size)
        fsize = world^.fieldsize
        allinitposes = case (world^.nowplayer) `mod` 4 of
            0 -> [(-1, States x (fsize^.height - turned^.height) trn) | x <- [0..(fsize^.width - turned^.width)]]
            1 -> [(-1, States x 0 trn) | x <- [0..(fsize^.width - turned^.width)]]
        iscol = \x -> not $ isCollision (Hub crd (snd x) []) (world^.field)
    in filter iscol allinitposes

-- srcがtargetに対して方向nで接続しているか
isConnect :: Hub -> Hub -> Int -> Bool
isConnect src target n =
    let targetConnector = (target^.card^.connector)!!((n + target^.states^.turn + 2) `mod` 4)
        srcConnector = (src^.card^.connector)!!((n + src^.states^.turn) `mod` 4)
    in (targetConnector < 0 && srcConnector /= 0) || (targetConnector /= 0 && srcConnector < 0) || targetConnector == srcConnector

-- バーストフラグ
isburst :: [Tree] -> Int -> Bool
isburst field low = or [burstCounter t /= Nothing && numofTreeCard t > low | t <- field]

-- 低バーストの木フラグ
isLowburstTree :: Tree -> Int -> Bool
isLowburstTree tree low = burstCounter tree /= Nothing && numofTreeCard tree <= low

-- バーストする木
burstfilter :: [Tree] -> Int -> [Tree]
burstfilter field low = filter (\t -> burstCounter t /= Nothing && numofTreeCard t > low ) field

-- 木ごとのバースト数
burstCounter tree =
    case tree of
        (Fork h) -> fmap (+1) $ fmap sum $ sequence $ map burstCounter (h^.trees)
        (Passage p) -> Nothing
        _ -> Just 0

-- フィールドオーバー判定
fieldover :: States -> Size -> Size -> Bool
fieldover st csize fsize = st^.posx < 0 || st^.posx + (normalizeCardSize (st^.turn) csize)^.width > fsize^.width || st^.posy < 0 || st^.posy + (normalizeCardSize (st^.turn) csize)^.height > fsize^.height

-- 衝突判定
--      Hub     : 置こうとしているハブ
--      tree    : 探索対象の木
isCollision :: Hub -> [Tree] -> Bool
isCollision h f =
    let normalized = normalizeCardSize (h^.states^.turn) (h^.card^.size)
    in True `elem ` [collisionCheck (h^.states) normalized t | t <- f]
collisionCheck :: States -> Size -> Tree -> Bool
collisionCheck (States sx sy st) ss tree =
    case tree of
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
connectedState (Hub c s _) (Hub c' s' _) n =
    let turnedPSize = normalizeCardSize (s'^.turn) (c'^.size)
        turnedCSize = normalizeCardSize (s^.turn) (c^.size)
        x' = case n of
            1 -> (s'^.posx) + turnedPSize^.width
            3 -> (s'^.posx) - turnedCSize^.width
            _ -> (s'^.posx) + ((turnedPSize^.width - turnedCSize^.width) `div` 2)
        y' = case n of
            0 -> (s'^.posy) - turnedCSize^.height
            2 -> (s'^.posy) + turnedPSize^.height
            _ -> (s'^.posy) + ((turnedPSize^.height - turnedCSize^.height) `div` 2)
    in States x' y' (s^.turn)

-- カードサイズの向きによる正規化
normalizeCardSize :: Int -> Size -> Size
normalizeCardSize t s = if even t then s else Size{_width = (s^.height), _height = (s^.width)}

-- 木のハッシュ
hashtree tree = case tree of
    Fork h -> hashlist [h^.states^.posx, h^.states^.posy]
    _ -> 1
hashlist (x:xs) =
    let fnv_offset_basis = 2166136261 :: Int
        fnv_prime = 16777619 :: Int
        hash t = (t * fnv_prime) `xor` x
    in if null xs
        then hash fnv_offset_basis
        else hash $ hashlist xs

-- 全てのDeadEndをPassageに変更
unDeadEnd :: [Tree] -> [Tree]
unDeadEnd field = map (\t -> unDeadEndTree t field) field
unDeadEndTree tree field = case tree of
    (Fork h) ->  Fork $ h&trees.~(map app $ zip (h^.trees) $ map (`mod` 4) [4-h^.states^.turn..]) where
        app (child, n) = case child of
            DeadEnd -> Passage ((moduloID n)!!0) where
                un = (unusedPassage field)
                len = length $ h^.card^.connector -- コネクタの数
                moduloID z = filter (\y -> y `mod` len==(z+len-(h^.states^.turn)) `mod` len) un -- コネクタの数を法とするxと等しい未使用パッセージの無限リスト
            _ -> unDeadEndTree child field
    _ -> tree

-- 特定ハッシュのhubをtreeから取得
getHashCard tree hash = case tree of
    Fork h -> if hash == hashtree tree
            then [h]
            else concatMap (`getHashCard` hash) (h^.trees)
    _ -> []

-- フィールドの特定hashのカードを操作
modifyCard hash field f =
    let childForks tree hash = case tree of
            Fork h -> if hash == hashtree tree
                    then filter extracttrees (h^.trees)
                    else concatMap (`childForks` hash) (h^.trees)
                where extracttrees t = case t of Fork _ -> True; _ -> False
            _ -> []
        childs = concatMap (`childForks` hash) field
        removed =  map (\x -> modifyCardbyTree x hash (\h -> DeadEnd)) field
        targetHub = head $ concatMap (`getHashCard` hash) field
    in unDeadEnd $ removed ++ childs ++ [f targetHub]

modifyCardbyTree tree hash f = case tree of
    Fork h -> if hash == hashtree tree
            then f h
            else Fork $ h&trees.~map (\x -> modifyCardbyTree x hash f) (h^.trees)
    _ -> tree

-- フィールドのカード枚数
numofFieldCard :: [Tree] -> Int
numofFieldCard field = sum $ map numofTreeCard field
numofTreeCard tree = case tree of
            Fork h -> 1 + numofFieldCard (h^.trees)
            _ -> 0

-- cardの周辺を再接続
reconnect :: Int -> [Tree]-> [Tree]
reconnect hash field =
    let mindex = head $ filter (\x -> not $ null $ getHashCard (field!!x) hash) [0..(length field) - 1] -- メインの木のインデックス
        mainHub = head $ concatMap (`getHashCard` hash) field
        isConnectTree tree n = case tree of -- 木が接続可能か判定
            Fork h -> if isConnect mainHub h n && connectedState h mainHub n == (h^.states) then True else or $ map (`isConnectTree` n) (h^.trees)
            _ -> False
        applicateTree n (srctree, ignore)
            | n >= 0 =
                let connectTree_index = filter (`isConnectTree` n) ignore
                in applicateTree (n - 1) $ if null connectTree_index
                    then (srctree, ignore)
                    else let tindex = head connectTree_index
                        in (modifyCardbyTree srctree hash (\h -> Fork (h&trees .~ take ((n + h^.states^.turn) `mod` (length $ h^.card^.connector)) (h^.trees)
                         ++ [tindex] ++ drop (1 + (n + h^.states^.turn) `mod` (length $ h^.card^.connector)) (h^.trees))), delete tindex ignore)
            | otherwise = (srctree, ignore)
        applicated = applicateTree (length (mainHub^.trees) - 1) (Fork mainHub, delete (Fork mainHub) field)
    in fst applicated : snd applicated