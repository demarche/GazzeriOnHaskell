module Magic.Description where
import Text.Parsec
import Data.List.Split
import Data.List
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
    let conlst = [up, right, down, left]
        conToStr con = case con of 6 -> "自由色"; 0 -> "接続不可"; 1 -> "赤"; 2 -> "青"; 3 -> "黒"; 4 -> "緑"; 5 -> "黄"
        conStr = if 1 == length (group conlst) then "全て" ++ (conToStr up) else concat $ intersperse "," $ map conToStr conlst
    return $ (show width) ++ "x" ++ (show height) ++ "," ++ conStr ++ "のカード"

burstparser = do
    canburst <- number
    return $ "※このカードはバーストすることが" ++ if canburst == 0 then "できない\n" else "できる\n"

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
    Text.Parsec.<|> do
    try $ string "usecard"
    spaces
    x <- cardparser
    spaces
    y <- burstparser
    return $ x ++ "をフィールドに置く．" ++ y
    Text.Parsec.<|> do
    try $ string "changefieldcard"
    spaces
    x <- cardparser
    spaces
    y <- burstparser
    return $ x ++ "に変化させる．" ++ y
    Text.Parsec.<|> do
    try $ string "changefielddirection"
    spaces
    dir <- number
    spaces
    canburst <- burstparser
    return $ (show $ 90 * dir) ++ "度回転させる．" ++ canburst

descriptionMagic :: String -> String
descriptionMagic str =  case parse description "desc" str of
        Right a -> a
        Left a -> show a

formatting funcs = do
    let concatdesc = concat $ map descriptionMagic funcs
        numbered = concat $ zipWith (\x y -> (show y) ++ "." ++ x ++ "\n") (init $ splitOn "\n" concatdesc) [1..]
    res <- insertNewline numbered 0
    return res