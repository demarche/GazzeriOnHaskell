module Magic.Func where

import Text.Parsec
import Header
import GazWorld
import Field
import FreeGame
import Control.Lens
import Control.Applicative ((<$>), (<*>), (<*), (*>))

fractalfix f (x:xs) s = f x (fractalfix f xs s)
fractalfix f [] s = s

number = read <$> many1 digit

magicinsts world = case world^.mode of
    Magic inst _ -> inst
    _ -> []

magicarg world = case world^.mode of
    Magic _ (Interpreter arg) -> arg
    _ -> []

cardparser = do
    string "card"
    spaces
    up <- number
    spaces
    right <- number
    spaces
    down <- number
    spaces
    left <- number
    spaces
    width <- number
    spaces
    height <- number
    return ModCard {_connector = [up, right, down, left], _image = 0, _size = Size width height}


-- 命令パーサー
instruct = do
    try $ string "select_fieldcard"
    spaces
    x <- number
    return $ \world -> world&mode.~(Magic (magicinsts world) (SelectFieldCard x []))
    Text.Parsec.<|> do
    try $ string "clearfield"
    return $ \world -> fractalfix (\x w -> (`resetinit2` [x]) $ updateField $ w&field.~modifyCard x (w^.field) (\h -> DeadEnd)) (magicarg world) world
    Text.Parsec.<|> do
    try $ string "incput"
    spaces
    x <- number
    return $ \world -> world&numofput.~x
    Text.Parsec.<|> do
    try $ string "removedeck"
    spaces
    x <- number
    return $ \world ->
        let now = world^.nowplayer
            nowdeck = (world^.decks)!!(world^.nowplayer)
        in world&decks.~ take now (world^.decks) ++ [drop (if x <= 0 then length nowdeck else min x (length nowdeck)) nowdeck] ++ drop (now + 1) (world^.decks)
    Text.Parsec.<|> do
    try $ string "turnend"
    return $ \world -> removeNowMagic $ world&mode.~(Burst 0 [] Draw)
    Text.Parsec.<|> do
    try $ string "myinit"
    spaces
    x <- number
    return $ \world ->
        let now = world^.nowplayer
            inits = world^.initiations
        in world&initiations.~ take now inits ++ [if x == 0 then Nothing else Just 0] ++ drop (now + 1) inits
    Text.Parsec.<|> do
    try $ string "removehand"
    spaces
    x <- number
    return $ \world ->
        let now = world^.nowplayer
            nowhand = (world^.handcards)!!(world^.nowplayer)
        in world&handcards.~ take now (world^.handcards) ++ [drop (if x <= 0 then length nowhand else min x (length nowhand)) nowhand] ++ drop (now + 1) (world^.handcards)
    Text.Parsec.<|> do
    try $ string "usecard"
    spaces
    x <- cardparser
    spaces
    canburst <- number
    return $ \world -> world&mode.~(Magic (magicinsts world) (SelectField [] x (canburst /= 0)))
    Text.Parsec.<|> do
    try $ string "changefieldcard"
    spaces
    cd <- cardparser
    spaces
    canburst <- number
    let toNot h = h&trees.~map (\x -> NotConnect) (h^.trees)
    return $ \world ->
        let putted = fractalfix (\x w -> w&field.~modifyCard x (w^.field) (\h -> Fork (toNot h&card.~cd))) (magicarg world) world
            updated = updateField putted
            low = world^.lowburst
            burst = detectLowHighburst (putted^.field) (updated^.field) low
        in if (not . fst) burst && (canburst /= 0 || (not . snd) burst) then updated else magicStart world (world^.selectedmagiccard)
    Text.Parsec.<|> do
    try $ string "changefielddirection"
    spaces
    dir <- number
    spaces
    canburst <- number
    let turncard x w =
            let forest = (modifyCard x (w^.field) (\h -> Fork h))
                extractHub (Fork h) = h
                lhub = extractHub $ last forest
            in reconnect x $ initiation (lhub^.card) ((lhub&(states.turn).~(dir + lhub^.states^.turn) `mod` (length $ lhub^.card^.connector))^.states) (init forest)
    return $ \world ->
        let putted = fractalfix (\x w -> w&field.~ turncard x w) (magicarg world) world
            updated = updateField putted
            low = world^.lowburst
            burst = (False, False)--detectLowHighburst (putted^.field) (updated^.field) low
        in if (not . fst) burst && (canburst /= 0 || (not . snd) burst) then updated else magicStart world (world^.selectedmagiccard)

defaulter :: World -> String -> World
defaulter world str = world&mode.~PError str

interpreterMagic :: String -> World -> World
interpreterMagic str =  case  parse instruct "inst" str of
        Right a -> a
        Left a -> (`defaulter` (show a))

