import System.Environment
import System.IO
import Control.Monad
import Data.Char

data Brain = Brain {
    ptail :: String,
    mem :: Char,
    memleft :: String,
    memright :: String,
    pst :: [String],
    out :: String
}

incrchr = (chr . incr255 . ord)
    where
        incr255 255 = 0
        incr255 x = (x+1)

decrchr = (chr . decr255 . ord)
    where
        decr255 0 = 255
        decr255 x = (x-1)

bfinput :: Brain -> IO Brain
bfinput brain = do
    c <- getChar
    return brain{ptail=tail (ptail brain),mem=c}

bfcore :: Brain -> (Brain, Bool)
bfcore brain@(Brain [] _ _ _ _ out) = (brain, True)
bfcore brain@(Brain (op:ptail) mem memleft memright pst out) =
    case op of
        '>' -> bfcore nextBrain{mem=head memright, memleft = mem:memleft, memright = tail memright}
        '<' -> bfcore nextBrain{mem=head memleft, memleft = tail memleft, memright = mem:memright}
        '+' -> bfcore nextBrain{mem=incrchr mem}
        '-' -> bfcore nextBrain{mem=decrchr mem}
        '.' -> bfcore nextBrain{out=mem:out}
        ',' -> (brain, False)
        '[' -> bfcore (if (ord mem) == 0 then
            nextBrain{ptail=matchParen 1 ptail}
            else nextBrain{pst=ptail:pst})
        ']' -> bfcore (if (ord mem) == 0 then
            nextBrain{pst=tail pst}
            else nextBrain{ptail=head pst})
    where
        nextBrain = brain{ptail=ptail}
        matchParen :: Int -> String -> String
        matchParen 0 ptail = ptail
        matchParen x ('[':ptail) = matchParen (x+1) ptail
        matchParen x (']':ptail) = matchParen (x-1) ptail
        matchParen x (_:ptail) = matchParen x ptail

bfmain :: Brain -> IO ()
bfmain brain = let (newbrain, complete) = bfcore brain in do
    putStr $ reverse $ out newbrain
    unless complete (bfinput newbrain{out=""} >>= bfmain)

main = do
    args <- getArgs
    src <- readFile $ head args
    bfmain (Brain (cleanprog src) (chr 0) "" [chr 0,chr 0..] [] "")
    where
        cleanprog = filter (\x -> elem x "><+-.,[]")
