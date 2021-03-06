{-# OPTIONS -fexcess-precision -funbox-strict-fields -O2#-}
import System.Environment
import Control.Monad
import Data.Word

data Brain = Brain {
	ptail :: String,
	mem :: Word8,
	memleft :: [Word8],
	memright :: [Word8],
	pst :: [String],
	out :: String
}

bfcore :: Brain -> (Brain, Bool)
bfcore brain@(Brain [] _ _ _ _ out) = (brain, True)
bfcore brain@(Brain (op:ptail) mem memleft memright pst out) =
	case op of
		'>' -> bfcore nextBrain{mem=head memright, memleft = mem:memleft, memright = tail memright}
		'<' -> bfcore nextBrain{mem=head memleft, memleft = tail memleft, memright = mem:memright}
		'+' -> bfcore nextBrain{mem=mem+1}
		'-' -> bfcore nextBrain{mem=mem-1}
		'.' -> bfcore nextBrain{out=(toEnum $ fromIntegral mem):out}
		',' -> (nextBrain, False)
		'[' -> bfcore (if mem == '\0' then
			nextBrain{ptail=matchParen 1 ptail}
			else nextBrain{pst=ptail:pst})
		']' -> bfcore (if mem == '\0' then
			nextBrain{pst=tail pst}
			else nextBrain{ptail=head pst})
	where
		nextBrain = brain{ptail=ptail}
		matchParen :: Int -> String -> String
		matchParen 0 ptail = ptail
		matchParen x ('[':ptail) = matchParen (x+1) ptail
		matchParen x (']':ptail) = matchParen (x-1) ptail
		matchParen x (_:ptail) = matchParen x ptail

bfinput :: Brain -> IO Brain
bfinput brain = do
	c <- getChar
	return brain{mem=(fromIntegral . fromEnum) c}

bfmain :: Brain -> IO ()
bfmain brain = let (newbrain, complete) = bfcore brain in do
	putStr $ reverse $ out newbrain
	unless complete $ bfinput newbrain{out=""} >>= bfmain

main = do
	src <- getArgs >>= readFile . head
	bfmain $ Brain (cleanprog src) '\0' ['\0','\0'..] ['\0','\0'..] [] ""
	where
		cleanprog = filter $ flip elem $ "><+-.,[]"
