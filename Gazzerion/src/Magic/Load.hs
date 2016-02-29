module Magic.Load where

import FreeGame
import Header
import Text.Parsec
import Data.List.Split
import Control.Lens
import Magic.Description
import System.Directory
import System.FilePath.Posix

numbered = read <$> many1 digit

chartodeck ch = case ch of 'S' -> S; 'A' -> A; 'B' -> B; 'C' -> C; 'D' -> D; 'E' -> Header.E

magicformat = do
    string "name="
    name <- Text.Parsec.many1 $ Text.Parsec.noneOf ['\n']
    newline
    string "cost="
    cost <- numbered
    newline
    string "deckcost="
    deckcost <- anyChar
    newline
    string "funcs="
    func <- Text.Parsec.many $ anyChar
    return MagicCard {_name = name, _cost = cost, _deckcost = (chartodeck deckcost), _funcs = (lines func), _desc = ""}


loadMagic path = do
    file <- embedIO $ readFile $ path </> "main.mgc"
    bmp <- readBitmap $ path </> "img.png"
    let mcard = case parse magicformat "load" file of
            Right a -> a&img.~bmp
            Left a -> MagicCard {_name = (show a), _cost = 0, _deckcost = S, _funcs = [], _desc = ""}
    mydesc <- formatting $ mcard^.funcs
    return $ mcard&desc.~mydesc

loadMagicAll :: Game [MagicCard]
loadMagicAll = do
    file <- embedIO $ getDirectoryContents "Magic"
    let ignored = ["Magic" </> x | x <- file, x /= ".", x /= ".."]
    mc <- mapM loadMagic ignored
    return mc