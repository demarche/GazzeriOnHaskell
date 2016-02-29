module Magic.Description where
import Text.Parsec
import Data.List.Split
import Foreign.C.String          (newCStringLen)
import FreeGame

sjisByteSize :: String -> IO Int
sjisByteSize s = snd <$> newCStringLen s

--insertNewline :: String -> Int -> String
insertNewline (x:xs) total = do
    mywid <- embedIO $ sjisByteSize [x]
    nextstr <- insertNewline xs (if x == '\n' then 0 else if mywid + total > 30 then 3 else total + mywid)
    if mywid + total > 30
        then return $ "\n　"++(x:nextstr)
        else return $ x:nextstr
insertNewline [] _ = return ""

number = read <$> many1 digit

description = do
    try $ string "select_fieldcard"
    spaces
    x <- many1 digit
    return $ "場のカードを" ++ x ++ "枚，"
    Text.Parsec.<|> do
    try $ string "clearfield"
    return "取り除く．\n"
    Text.Parsec.<|> do
    try $ string "incput"
    spaces
    x <- many1 digit
    return $ "このターンは手札を" ++ x ++ "枚場に出すことが出来る．\n"
    Text.Parsec.<|> do
    try $ string "removedeck"
    spaces
    x <- number
    return $ "自分の山札を" ++ (if x <= 0 then "全て" else (show x) ++ "枚") ++ "捨てる．\n"
    Text.Parsec.<|> do
    try $ string "turnend"
    return $ "ターンを終了する．\n"
    Text.Parsec.<|> do
    try $ string "myinit"
    spaces
    x <- many1 digit
    return $ "自分のイニシエーション権利が"++ (if x == "0" then "復活" else "消滅") ++"する．\n"
    Text.Parsec.<|> do
    try $ string "removehand"
    spaces
    x <- number
    return $ "自分の手札を" ++ (if x <= 0 then "全て" else (show x)  ++ "枚") ++ "捨てる．\n"

descriptionMagic :: String -> String
descriptionMagic str =  case  parse description "desc" str of
        Right a -> a
        Left a -> show a

formatting funcs = do
    let concatdesc = concat $ map descriptionMagic funcs
        numbered = concat $ zipWith (\x y -> (show y) ++ "." ++ x ++ "\n") (init $ splitOn "\n" concatdesc) [1..]
    res <- insertNewline numbered 0
    return res