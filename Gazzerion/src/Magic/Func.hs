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

-- 命令パーサー
instruct = do
    try $ string "select_fieldcard"
    spaces
    x <- number
    return $ \world -> world&mode.~(Magic (magicinsts world) (SelectField x []))
    Text.Parsec.<|> do
    try $ string "clearfield"
    return $ \world -> fractalfix (\x w -> (`resetinit` [x]) $ updateField $ w&field.~removeCard x (w^.field)) (magicarg world) world
    Text.Parsec.<|> do
    try $ string "incput"
    spaces
    x <- number
    return $ \world -> removeNowMagic $ world&mode.~(Choice x)
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
    return $ \world -> removeNowMagic $ world&mode.~(Burst 0 [])
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

defaulter :: World -> World
defaulter world = world&mode.~PError

interpreterMagic :: String -> World -> World
interpreterMagic str =  case  parse instruct "inst" str of
        Right a -> a
        Left _ -> defaulter

