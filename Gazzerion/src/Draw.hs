module Draw where
import Header
import Card
import Field
import FreeGame
import Control.Lens
import Control.Monad.State

itod :: Int -> Double
itod n = (fromIntegral n) :: Double
dtoi :: Double -> Int
dtoi n = (truncate n) :: Int

v2Int x y = V2 ((fromIntegral x) :: Double) ((fromIntegral y) :: Double)

drawBox x y w h = polygon [v2Int x y, v2Int (x+w) y, v2Int (x+w) (y+h), v2Int x (y+h)]

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

drawCardBack world = do
    color black $ polygon [v2Int 0 0, v2Int wid 0, v2Int wid hei, v2Int 0 hei]
    color white $ polygonOutline [v2Int 0 0, v2Int wid 0, v2Int wid hei, v2Int 0 hei]where
        wid = world^.cardsizeMax^.width * world^.enviroment^.grid
        hei = world^.cardsizeMax^.height * world^.enviroment^.grid

fixturn v2 t wid hei = V2 (fx (v2^._x)) (fy (v2^._y)) where
    fx | t == 3 = (+ (itod hei))
       | t == 2 = (+ (itod wid))
       | otherwise = (+0)
    fy | t == 1 = (+ (itod wid))
       | t == 2 = (+ (itod hei))
       | otherwise = (+0)

drawTree tree env = case tree of
    Fork c s t -> do
        translate (fixturn (v2Int mx my) (s^.turn) wid hei) $ rotateD deg $ drawCard (_card tree) env
        forM_ t $ \u -> drawTree u env where
            deg = itod $ 360 * s^.turn `div` length (c^.connector)
            wid = c^.size^.width * env^.grid
            hei = c^.size^.height * env^.grid
            mx = s^.posx * env^.grid + env^.gridx
            my = s^.posy * env^.grid + env^.gridy
    _ -> line[V2 0 0, V2 0 0]

drawGrid world = do
    let env = world^.enviroment
        fsize = world^.fieldsize
    forM_ [0..fsize^.width] $ \x -> color black $ line [v2Int (env^.gridx + env^.grid * x) (env^.gridy), v2Int (env^.gridx + env^.grid * x) (env^.gridy + env^.gridh)]
    forM_ [0..fsize^.height] $ \y -> color black $ line [v2Int (env^.gridx) (env^.gridy + env^.grid * y), v2Int (env^.gridx + env^.gridw) (env^.gridy + env^.grid * y)]

drawMovingCard world = do
    mp <- mousePosition
    let nowcard = ((world^.handcards)!!(world^.nowplayer))!!(world^.selectedhandcard)
        env = world^.enviroment
        fsize = world^.fieldsize
        cardscale = v2Int (world^.enviroment^.grid * nowcard^.size^.width) (world^.enviroment^.grid * nowcard^.size^.height)
        cardpos = if even (world^.nowturn) then mp - 0.5 * cardscale else mp - 0.5 * (V2 (cardscale^._y) (cardscale^._x))
        fix_size = if even (world^.nowturn) then Size (nowcard^.size^.width-1) (nowcard^.size^.height-1) else Size (nowcard^.size^.height-1) (nowcard^.size^.width-1)
        deg = itod $ 360 * (world^.nowturn) `div` length (nowcard^.connector)
        wid = nowcard^.size^.width * env^.grid
        hei = nowcard^.size^.height * env^.grid
        movedpos = fixturn cardpos (world^.nowturn) wid hei -- 調整後の座標
        x = dtoi (cardpos^._x) -  env^.gridx;y = dtoi (cardpos^._y) - env^.gridy
        fieldpt = v2Int (x `div` env^.grid) (y `div` env^.grid)
        fitted = if x >= 0 && y >= 0 && x < env^.grid * (fsize^.width - fix_size^.width) && y < env^.grid * (fsize^.height - fix_size^.height)
            then fixturn (fieldpt ^* itod (env^.grid) + (v2Int (env^.gridx) (env^.gridy))) (world^.nowturn) wid hei
            else movedpos
    translate fitted $ rotateD deg $ drawCard nowcard env
    return $ States (dtoi (fieldpt^._x)) (dtoi (fieldpt^._y)) (world^.nowturn)

drawPlayerCards :: StateT World (StateT World Game) Int
drawPlayerCards = do
    csize <- use cardsizeMax
    maxh <- use maxhandcards
    maxm <- use maxmagiccards
    maxp <- use maxplayer
    ssize <- use screensize
    env <- use enviroment
    deckis <- use decks
    hcards <- use handcards
    myfont <- use font
    now <- use nowplayer
    nhk <- use selectedhandcard
    mpos <- mousePosition
    let charsize :: Double
        charsize = sqrt $ fromIntegral (2 * csize^.width * csize^.height * env^.grid)
        charhlf =  V2 (charsize * 0.3) (charsize * (-0.2))
        getCardsWidth n = env^.grid * (csize^.width * (maxh!!n + maxm!!n) + 4) -- n番目のプレイヤーのカードの総width
        getmodwid x n = foldl (+) 0 [getCardsWidth i | i <- [0..n-1], i `mod` 4 == x] -- n番目までのx (mod 4) 番テーブルのプレイヤーの横幅の合計
        leftup x = (ssize^.width - getmodwid (x `mod` 4) maxp) `div` 2 -- x番目のプレイヤーの左上X座標
        deckstr n = show $ length $ deckis!!n -- n番目のプレイヤーのデッキ枚数残り
    ret <- forM [0..maximum [3, maxp-1]] $ \i ->
        case i `mod` 4 of
            0 -> do
                let myx = (leftup i) + getmodwid (i `mod` 4) i
                    myy = env^.gridy + env^.gridh
                    mywid = getCardsWidth i
                    myhei = (csize^.height + 2) * env^.grid
                    myhk = hcards!!i
                color (if now == i then cyan else gray) $ drawBox myx myy mywid myhei--polygon [v2Int myx myy, v2Int (myx + mywid) myy, v2Int (myx + mywid) (myy + myhei), v2Int myx (myy + myhei)]
                translate (v2Int (myx + env^.grid) (myy + env^.grid)) $ drawCardBack =<< get -- 山札
                color white $
                 translate (v2Int (myx + env^.grid * (1 + csize^.width `div` 2)) (myy + env^.grid * (1 + csize^.height `div` 2)) - charhlf) $ text myfont charsize (deckstr i)
                let fx x = myx + env^.grid * (2 + csize^.width * (1 + x)) -- x枚目の手札の左上x座標
                    fy = myy + env^.grid    -- 手札の左上y座標
                forM_ [0..length(myhk)-1] $ \x -> when (i == now && x /= nhk || i /= now) $ translate (v2Int (fx x) fy) $ drawCard (myhk!!x) env -- 手札描画
                let ptlst = [n | n <- [0..length(hcards!!i)-1], -- カーソルの位置取得
                        (itod . fx) n <= mpos^._x && itod (env^.grid * (myhk!!n)^.size^.width + fx n) > mpos^._x && itod fy <= mpos^._y && itod (env^.grid * (myhk!!n)^.size^.height + fy) > mpos^._y]
                    pt = if null ptlst then -1 else ptlst!!0
                let wid = (myhk!!pt)^.size^.width * env^.grid
                    hei = (myhk!!pt)^.size^.height * env^.grid
                when (pt >= 0) $ color green $ translate (v2Int (fx pt) fy) $ polygonOutline [v2Int 0 0, v2Int wid 0, v2Int wid hei, v2Int 0 hei]-- 光らせる
                return pt
            1 -> do
                let myx = (leftup i) + getmodwid (i `mod` 4) i
                    myy = 0
                    mywid = getCardsWidth i
                    myhei = (csize^.height + 2) * env^.grid
                    myhk = hcards!!i
                color (if now == i then cyan else gray) $ drawBox myx myy mywid myhei
                translate (v2Int (myx + mywid - env^.grid * (1 + csize^.width)) (myy + env^.grid)) $ drawCardBack =<< get -- 山札
                color white $
                 translate (v2Int (myx  + mywid - env^.grid * (1 + csize^.width `div` 2)) (myy + env^.grid * (1 + csize^.height `div` 2)) - charhlf) $ text myfont charsize (deckstr i)
                let fx x = myx + mywid - env^.grid * (2 + csize^.width * (2 + x)) -- x枚目の手札の左上x座標
                    fy = myy + env^.grid    -- 手札の左上y座標
                forM_ [0..length(myhk)-1] $ \x -> when (i == now && x /= nhk || i /= now) $ translate (v2Int (fx x) fy) $ drawCard (myhk!!x) env -- 手札描画
                let ptlst = [n | n <- [0..length(hcards!!i)-1], -- カーソルの位置取得
                        (itod . fx) n <= mpos^._x && itod (env^.grid * (myhk!!n)^.size^.width + fx n) > mpos^._x && itod fy <= mpos^._y && itod (env^.grid * (myhk!!n)^.size^.height + fy) > mpos^._y]
                    pt = if null ptlst then -1 else ptlst!!0
                let wid = (myhk!!pt)^.size^.width * env^.grid
                    hei = (myhk!!pt)^.size^.height * env^.grid
                when (pt >= 0) $ color green $ translate (v2Int (fx pt) fy) $ polygonOutline [v2Int 0 0, v2Int wid 0, v2Int wid hei, v2Int 0 hei]-- 光らせる
                return pt
            _ -> return $ -1
    return $ ret!!now