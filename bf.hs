-- Pure, input supplied on first line, ending with $. Programs which read beyond end of input will crash

import System.IO
import Data.Char

data Brain = Brain {
    ptail :: String,
    opc :: Int,
    mem :: Char,
    memleft :: String,
    memright :: String,
    pst :: [String],
    out :: String,
    input :: String
}

incrchr = (chr . incr255 . ord)
    where
        incr255 255 = 0
        incr255 x = (x+1)

decrchr = (chr . decr255 . ord)
    where
        decr255 0 = 255
        decr255 x = (x-1)

bfinterp :: (String, String) -> String
bfinterp (input, prog) = bfcore (Brain cleanprog 1 (chr 0) "" [chr 0,chr 0..] [] "" input)
    where
        cleanprog = filter (\x -> elem x "><+-.,[]") prog
        bfcore :: Brain -> String
        bfcore (Brain [] _ _ _ _ _ out _) = reverse out
        bfcore brain@(Brain (op:ptail) opc mem memleft memright pst out input) =
            if opc>100000 then (reverse out) ++ "\nPROCESS TIME OUT. KILLED!!!"
            else case op of
                '>' -> bfcore nextBrain{mem=head memright, memleft = mem:memleft, memright = tail memright}
                '<' -> bfcore nextBrain{mem=head memleft, memleft = tail memleft, memright = mem:memright}
                '+' -> bfcore nextBrain{mem=incrchr mem}
                '-' -> bfcore nextBrain{mem=decrchr mem}
                '.' -> bfcore nextBrain{out=mem:out}
                ',' -> bfcore nextBrain{mem=head input, input=tail input}
                '[' -> bfcore (if (ord mem) == 0 then
                    nextBrain{ptail=matchParen 1 ptail,opc=opc+2}
                    else nextBrain{pst=ptail:pst})
                ']' -> bfcore (if (ord mem) == 0 then
                    nextBrain{pst=tail pst}
                    else nextBrain{ptail=head pst,opc=opc+2})
            where
                nextBrain = brain{opc=opc+1,ptail=ptail}
                matchParen :: Int -> String -> String
                matchParen 0 ptail = ptail
                matchParen x ('[':ptail) = matchParen (x+1) ptail
                matchParen x (']':ptail) = matchParen (x-1) ptail
                matchParen x (_:ptail) = matchParen x ptail

dropfirstline :: String -> String
dropfirstline ('\n':str) = str
dropfirstline (_:str) = dropfirstline str

getInputAndProg :: String -> String -> (String, String)
getInputAndProg input ('$':'\n':prog) = ((reverse input), prog)
getInputAndProg input (x:prog) = getInputAndProg (x:input) prog

bfread :: String -> String
bfread = bfinterp . (getInputAndProg "") . dropfirstline

main = interact bfread