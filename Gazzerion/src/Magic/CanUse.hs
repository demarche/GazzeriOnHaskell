module Magic.CanUse where
import Text.Parsec
import Field
import Control.Lens
import Header
import FreeGame
import Data.List

number = read <$> many1 digit

canuse = do
    try $ string "select_fieldcard"
    spaces
    x <- number
    return $ \world -> numofFieldCard (world^.field) >= x
    Text.Parsec.<|> do
    try $ string "removedeck"
    spaces
    number
    return $ \world -> length ((world^.decks)!!(world^.nowplayer)) >= 1
    Text.Parsec.<|> do
    try $ string "myinit"
    spaces
    x <- number
    return $ \world ->
        let myinit = (world^.initiations)!!(world^.nowplayer)
        in if x == 0 then myinit /= Nothing else myinit == Nothing
    Text.Parsec.<|> do
    try $ string "removehand"
    spaces
    number
    return $ \world -> length ((world^.handcards)!!(world^.nowplayer)) >= 1

canuseMagic :: MagicCard -> World -> Bool
canuseMagic magic world =
    let canusefunc :: String -> Bool
        canusefunc func = case  parse canuse "canuse" func of
            Right f -> f world
            Left _ -> True
    in all canusefunc (magic^.funcs)
