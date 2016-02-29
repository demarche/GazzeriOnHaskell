module Draw where
import Header
import Card
import Field
import FreeGame
import Control.Lens
import Control.Monad.State
import FreeGame.Data.Font
import Data.Maybe

itod :: Int -> Double
itod n = (fromIntegral n) :: Double
itof :: Int -> Float
itof n = (fromIntegral n) :: Float
dtoi :: Double -> Int
dtoi n = (truncate n) :: Int

v2Int x y = V2 ((fromIntegral x) :: Double) ((fromIntegral y) :: Double)

drawBox x y w h = polygon [v2Int x y, v2Int (x+w) y, v2Int (x+w) (y+h), v2Int x (y+h)]
drawBoxOutLine x y w h = polygonOutline [v2Int x y, v2Int (x+w) y, v2Int (x+w) (y+h), v2Int x (y+h)]
cardScale csize world = Size (csize^.width * world^.enviroment^.grid)  (csize^.height * world^.enviroment^.grid)

-- カードを描画
drawCard card env = do
    color gray $ polygon [V2 0 0, V2 wid 0, V2 wid hei, V2 0 hei]
    color black $ polygonOutline [V2 0 0, V2 wid 0, V2 wid hei, V2 0 hei]
    forM_ [0..len-1] $ \i -> unless ((card^.connector)!!i==0) $ color (colors!!((card^.connector)!!i-1)) $ translate (conPos!!i) $ polygon[V2 0 0, V2 cGrid 0, V2 cGrid cGrid, V2 0 cGrid] where
        wid = itod $ card^.size^.width * env^.grid
        hei = itod $ card^.size^.height * env^.grid
        len = length (card^.connector)
        cGrid =  sqrt $ fromIntegral $ (card^.size^.width * card^.size^.height * ((env^.grid) ^ 2)) `div` 16
        colors = [red, blue, black, green, yellow]
        conPos = [V2 ((wid - cGrid) * 0.5) 0, V2 (wid - cGrid) ((hei - cGrid) * 0.5), V2 ((wid - cGrid) * 0.5) (hei - cGrid), V2 0 ((hei - cGrid) * 0.5)]

-- 魔法カードを描画
drawMagicCard card world = do
    let csize = cardScale (Size 4 6) world
        fontsize = sqrt $ fromIntegral $ (csize^.width * csize^.height) `div` 230
        env = world^.enviroment
        bsize = bitmapSize (card^.img)
        drawframe x y w h = do
            color (V4 0.95 0.95 0.95 1) $ drawBox x y w h
            color (V4 0.85 0.85 0.85 1) $ drawBoxOutLine x y w h
    color white $ drawBox 0 0 (csize^.width) (csize^.height)
    color black $ drawBoxOutLine 0 0 (csize^.width) (csize^.height)
    drawframe (env^.grid `div` 4) (env^.grid `div` 4) (csize^.width - env^.grid) (env^.grid `div` 2)
    color black $ translate (v2Int (env^.grid `div` 4) (env^.grid `div` 4 * 3)) $ text (world^.font) fontsize (card^.name)
    drawframe (csize^.width - env^.grid `div` 4 * 3) (env^.grid `div` 4) (env^.grid `div` 2) (env^.grid `div` 2)
    color black $ translate (v2Int (csize^.width - env^.grid `div` 4 * 3) (env^.grid `div` 4 * 3)) $ text (world^.font) fontsize (show $ card^.cost)
    let imgscale = V2 (itod (csize^.width - env^.grid `div` 2 ) / itod (fst bsize)) (itod (env^.grid * 3) / itod (snd bsize))
    translate (v2Int (csize^.width `div` 2) ((csize^.height - env^.grid) `div` 2)) $ scale imgscale $ bitmap (card^.img)
    drawframe (env^.grid `div` 4) (env^.grid * 4 - env^.grid `div` 4) (csize^.width - env^.grid `div` 2) (env^.grid * 2)
    color black $ translate (v2Int (env^.grid `div` 4) (env^.grid * 4 + env^.grid `div` 4)) $ text (world^.font) fontsize (card^.desc)

-- カードの裏面を描画
drawCardBack world = do
    color black $ drawBox 0 0 wid hei
    color white $ drawBoxOutLine 0 0 wid hei where
        wid = world^.cardsizeMax^.width * world^.enviroment^.grid
        hei = world^.cardsizeMax^.height * world^.enviroment^.grid

-- 回転後のカードの座標を修正
fixturn v2 t wid hei = V2 (fx (v2^._x)) (fy (v2^._y)) where
    fx | t == 3 = (+ (itod hei))
       | t == 2 = (+ (itod wid))
       | otherwise = (+0)
    fy | t == 1 = (+ (itod wid))
       | t == 2 = (+ (itod hei))
       | otherwise = (+0)

-- フィールドに配置済みの座標
translate' card st env y = (f . g) y  where
    deg = itod $ 360 * st^.turn `div` length (card^.connector)
    wid = card^.size^.width * env^.grid
    hei = card^.size^.height * env^.grid
    mx = st^.posx * env^.grid + env^.gridx
    my = st^.posy * env^.grid + env^.gridy
    f x = translate (fixturn (v2Int mx my) (st^.turn) wid hei) x
    g x = rotateD deg x

-- ギャザリオンの木を描画
drawTree tree env = case tree of
    Fork h -> do
        translate' (h^.card) (h^.states) env $ drawCard (h^.card) env
        forM_ (zip (h^.trees) [0..]) $ \u -> drawnest u where
            drawnest (t, n) = case t of
                Fork _ -> do
                    color gray $ debugTriangle n
                    drawTree t env
                DeadEnd -> do color black $ debugTriangle n; color white $ translate (V2 1 1) $ debugTriangle n
                NotConnect -> do color red $ debugTriangle n; color blue $ translate (V2 1 1) $ debugTriangle n
                _ -> line[V2 0 0, V2 0 0]
            wid = itod $ h^.card^.size^.width * env^.grid
            hei = itod $ h^.card^.size^.height * env^.grid
            conPos = [V2 ((wid - cGrid) * 0.5) 0, V2 (wid - cGrid) ((hei - cGrid) * 0.5), V2 ((wid - cGrid) * 0.5) (hei - cGrid), V2 0 ((hei - cGrid) * 0.5)]
            cGrid =  sqrt $ fromIntegral $ (h^.card^.size^.width * h^.card^.size^.height * ((env^.grid) ^ 2)) `div` 16
            debugTriangle n = translate' (h^.card) (h^.states) env $ translate (conPos!!n) $ do line[V2 0 0, V2 cGrid cGrid]; line[V2 cGrid 0, V2 0 cGrid]
    _ -> line[V2 0 0, V2 0 0]

-- グリッド線を描画
drawGrid world = do
    let env = world^.enviroment
        fsize = world^.fieldsize
    forM_ [0..fsize^.width] $ \x -> color black $ line [v2Int (env^.gridx + env^.grid * x) (env^.gridy), v2Int (env^.gridx + env^.grid * x) (env^.gridy + env^.gridh)]
    forM_ [0..fsize^.height] $ \y -> color black $ line [v2Int (env^.gridx) (env^.gridy + env^.grid * y), v2Int (env^.gridx + env^.gridw) (env^.gridy + env^.grid * y)]

-- 移動中のカードを描画
drawMovingCard world = do
    mp <- mousePosition
    let nowcard = ((world^.handcards)!!(world^.nowplayer))!!(world^.selectedhandcard)
        env = world^.enviroment
        fsize = world^.fieldsize
        Size wid hei = cardScale (nowcard^.size) world
        v2cardscale = v2Int wid hei
        cardpos = if even (world^.nowturn) then mp - 0.5 * v2cardscale else mp - 0.5 * (V2 (v2cardscale^._y) (v2cardscale^._x))
        fix_size = if even (world^.nowturn) then Size (nowcard^.size^.width-1) (nowcard^.size^.height-1) else Size (nowcard^.size^.height-1) (nowcard^.size^.width-1)
        deg = itod $ 360 * (world^.nowturn) `div` length (nowcard^.connector)
        movedpos = fixturn cardpos (world^.nowturn) wid hei -- 調整後の座標
        x = dtoi (cardpos^._x) -  env^.gridx;y = dtoi (cardpos^._y) - env^.gridy
        fieldpt = v2Int (x `div` env^.grid) (y `div` env^.grid)
        fitted = if x >= 0 && y >= 0 && x < env^.grid * (fsize^.width - fix_size^.width) && y < env^.grid * (fsize^.height - fix_size^.height)
            then fixturn (fieldpt ^* itod (env^.grid) + (v2Int (env^.gridx) (env^.gridy))) (world^.nowturn) wid hei
            else movedpos
    translate fitted $ rotateD deg $ drawCard nowcard env
    return $ States (dtoi (fieldpt^._x)) (dtoi (fieldpt^._y)) (world^.nowturn)

-- 置ける場所をハイライト
drawCanput world = do
    let nowhcardindex = world^.selectedhandcard
        nowcard = (world^.handcards)!!(world^.nowplayer)!!nowhcardindex
        scale = cardScale (nowcard^.size) world
    when (nowhcardindex >= 0) $
        forM_ (filter (\t -> t^.turn == world^.nowturn) $ map snd $ (world^.canputs)!!nowhcardindex) $ \x ->
            color (V4 1.0 0 0 0.5) $ translate' nowcard x (world^.enviroment) $ drawBox 0 0 (scale^.width) (scale^.height)

-- カードの土台表示
drawbasement world = do
    let env = world^.enviroment
        maxp = world^.maxplayer
        csize = cardScale (world^.cardsizeMax) world
    forM_ [0..maxp-1] $ \n ->
        let start = fst $ (env^.basestart)!!n
            size = snd $ (env^.basestart)!!n
            deg = case n of 0 -> 0; 1 -> 180
            decksize = length $ (world^.decks)!!n
            fnt = world^.font
            fontsize = sqrt (fromIntegral ( csize^.width * csize^.height `div` 16)) :: Double
            sizefix = dtoi $ fontsize * (metricsAscent fnt - metricsDescent fnt) * 0.5
            drawbase = do
                color (if world^.nowplayer == n then cyan else gray) $ polygon [V2 0 0, V2 (size^._x) 0, size, V2 0 (size^._y)]
                translate (v2Int (env^.grid) (env^.grid)) $ drawCardBack world
                translate (v2Int (env^.grid + csize^.width `div` 2 - sizefix) (env^.grid + csize^.height `div` 2 )) $ text fnt fontsize (show decksize)
        in translate start $ rotateD deg $ drawbase

-- 手札表示
drawhandcard world = do
    mpos <- mousePosition
    let env = world^.enviroment
        maxp = world^.maxplayer
        nowp = world^.nowplayer
        csize = cardScale (world^.cardsizeMax) world
        selected = world^.selectedhandcard
    focus <- forM [0..maxp-1] $ \n -> do
        let myhcard = (world^.handcards)!!n
            start = fst $ (env^.basestart)!!n
            offset = v2Int (csize^.width + env^.grid * 2) (env^.grid)
            cardpos x = case n `mod` 4 of
                0 -> start + offset + (v2Int (x * csize^.width) 0)
                1 -> start - offset - (v2Int ((x + 1) * csize^.width) (csize^.height))
        focusMaybe <- forM [0..(length myhcard) - 1] $ \c -> do
            let cannotput = nowp == n && if c >= length (world^.canputs) then True else null ((world^.canputs)!!c)
            when (n == nowp && c /= selected || n /= nowp) $ translate (cardpos c) $ drawCard (myhcard!!c) env -- 移動中じゃないカードだけ表示
            when cannotput $ blendMode Multiply $ color (V4 0.6 0.6 0.6 1) $ translate (cardpos c) $ drawBox 0 0 (csize^.width) (csize^.height) -- おけないカードを暗くする
            let normal = mpos - (cardpos c)
                isfocus = normal^._x >= 0 && normal^._y >=0 && normal^._x < itod (csize^.width) && normal^._y < itod (csize^.height)
            if isfocus && nowp == n && not cannotput
                then do
                    blendMode Add $ color (V4 0.1 0.1 0.1 1) $ translate (cardpos c) $ drawBox 0 0 (csize^.width) (csize^.height) -- 選択してるカードを明るくする
                    return $ Just c
                else return Nothing
        return $ mapMaybe (\x -> x) focusMaybe
    return $ if null $ concat focus then Nothing else Just $ head $ concat focus

-- 魔法カード表示
drawhandmagiccard world = do
    mpos <- mousePosition
    let env = world^.enviroment
        maxp = world^.maxplayer
        nowp = world^.nowplayer
        csize = cardScale (world^.cardsizeMax) world
        selected = world^.selectedmagiccard
    focus <- forM [0..maxp-1] $ \n -> do
        let mymcard = (world^.magiccards)!!n
            start = fst $ (env^.basestart)!!n
            offset = v2Int ((csize^.width * (1 + (world^.maxhandcards)!!n)) + env^.grid * 3) (env^.grid)
            cardpos x = case n `mod` 4 of
                0 -> start + offset + (v2Int (x * csize^.width) 0)
                1 -> start - offset - (v2Int ((x + 1) * csize^.width) (csize^.height))
        focusMaybe <- forM [0..(length mymcard) - 1] $ \c -> do
            let cannotput = nowp /= n || null (world^.canmagic) || not ((world^.canmagic)!!n!!c)
            when (n == nowp && c /= selected || n /= nowp) $ translate (cardpos c) $ drawMagicCard (mymcard!!c) world--drawCardBack world -- 移動中じゃないカードだけ表示
            when cannotput $ blendMode Multiply $ color (V4 0.6 0.6 0.6 1) $ translate (cardpos c) $ drawBox 0 0 (csize^.width) (csize^.height) -- おけないカードを暗くする
            let normal = mpos - (cardpos c)
                isfocus = normal^._x >= 0 && normal^._y >=0 && normal^._x < itod (csize^.width) && normal^._y < itod (csize^.height)
            if isfocus && nowp == n && not cannotput
                then do
                    blendMode Add $ color (V4 0.1 0.1 0.1 1) $ translate (cardpos c) $ drawBox 0 0 (csize^.width) (csize^.height) -- 選択してるカードを明るくする
                    return $ Just c
                else return Nothing
        return $ mapMaybe (\x -> x) focusMaybe
    return $ if null $ concat focus then Nothing else Just $ head $ concat focus

-- メッセージ表示
drawnotice world str col count =
    let env = world^.enviroment
        now = world^.nowplayer
        fnt = world^.font
        start = fst $ (env^.basestart)!!now
        deg = case now of 0 -> 0; 1 -> 180
        drawstr = do
            color (col - (V4 0 0 0 (itof count * 0.02))) $ translate (V2 0 (- itod count)) $ text fnt (0.04 * itod (world^.screensize^.width)) str
    in translate start $ rotateD deg $ drawstr

-- バーストする木の表示
drawBurstTree tree env count  = case tree of
    Fork h -> do
        let scale = Size (h^.card^.size^.width * env^.grid)  (h^.card^.size^.height * env^.grid)
        color (red - (V4 0 0 0 (itof count * 0.02))) $ translate' (h^.card) (h^.states) env $ drawBox 0 0 (scale^.width) (scale^.height)
        forM_ (h^.trees) $ \u -> drawBurstTree u env count
    _ -> line[V2 0 0, V2 0 0]

-- カーソルが乗っているカードを探す
selectedCardHash world = do
    mpos <- mousePosition
    let env = world^.enviroment
        search tree = case tree of
            Fork h -> let csize = cardScale (h^.card^.size) world
                          normal = mpos - v2Int (env^.gridx + env^.grid * h^.states^.posx) (env^.gridy + env^.grid * h^.states^.posy)
                in if normal^._x >= 0 && normal^._y >= 0 && normal^._x < (itod $ csize^.width) && normal^._y < (itod $ csize^.height)
                        then [hashtree tree]
                        else concat $ map search (h^.trees)
            _ -> []
        result = concat $ map search (world^.field)
    unless (null result) $ glowCardGHash (head result) world
    return $ if null result then Nothing else Just $ head result

-- hashのフィールドカードを光らす
glowCardGHash hash world =
    let glowTree tree hash world = case tree of
            Fork h -> if  hash == hashtree tree
                then  blendMode Add $ color (V4 0.1 0.1 0.1 1) $ translate' (h^.card) (h^.states) (world^.enviroment) $ drawBox 0 0 (csize^.width) (csize^.height)
                else mapM_ (\x -> glowTree x hash world) (h^.trees)
                where csize = cardScale (h^.card^.size) world
            _ ->  line[V2 0 0, V2 0 0]
    in mapM_ (\x -> glowTree x hash world) (world^.field)