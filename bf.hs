{-# OPTIONS -fexcess-precision -funbox-strict-fields -O2#-}
import System.Environment
import Control.Monad
import qualified Data.Array.Unboxed as UA
import Data.Bits
import Data.List
import Data.Word

data Brain = Brain {
	prog :: UA.UArray Int Word32,
	pc :: Int,
	mem :: Word8,
	memleft :: [Word8],
	memright :: [Word8],
	out :: String
}

data BrainParse = BrainParse {
	srctail :: String,
	lastc :: Char,
	idx :: Word32,
	jmptbl :: [Word32],
	code :: [Word32]
}

shiftright, shiftleft :: Brain -> Word32 -> Brain
shiftleft brain 0 = brain
shiftleft brain@(Brain _ _ mem memleft memright _) x = shiftleft brain{mem=head memleft, memleft = tail memleft, memright = mem:memright} (x-1)

shiftright brain 0 = brain
shiftright brain@(Brain _ _ mem memleft memright _) x = shiftright brain{mem=head memright, memleft = mem:memleft, memright = tail memright} (x-1)

bfcore :: Brain -> (Brain, Bool)
bfcore brain@(Brain prog pc mem memleft memright out) =
	if pc > (snd . UA.bounds) prog then (brain, True)
	else case op of
		0 -> bfcore $ shiftright nextBrain arg
		1 -> bfcore $ shiftleft nextBrain arg
		2 -> bfcore nextBrain{mem=mem+(fromIntegral arg)}
		3 -> bfcore nextBrain{mem=mem-(fromIntegral arg)}
		4 -> bfcore nextBrain{out=(toEnum $ fromIntegral mem):out}
		5 -> (nextBrain, False)
		6 -> bfcore (if mem == 0 then
			nextBrain{pc=fromIntegral arg}
			else nextBrain)
		7 -> bfcore (if mem == 0 then
			nextBrain
			else nextBrain{pc=fromIntegral arg})
	where
		word = prog UA.! pc
		op = (fromIntegral word) :: Word8
		arg = unsafeShiftR word 8
		nextBrain = brain{pc=pc+1}

bfinput :: Brain -> IO Brain
bfinput brain = do
	c <- getChar
	return brain{mem=(fromIntegral . fromEnum) c}

prInst :: Word32 -> String
prInst w =
	(case w .&. 255 of
		0 -> '>'
		1 -> '<'
		2 -> '+'
		3 -> '-'
		4 -> '.'
		5 -> ','
		6 -> '['
		7 -> ']'):' ':(show $ unsafeShiftR w 8)

prProg :: UA.UArray Int Word32 -> Int -> IO ()
prProg x i =
	if i > (snd . UA.bounds) x then return ()
	else putStrLn ((show i) ++ "\t" ++ (prInst $ x UA.! i)) >> prProg x (i+1)

bfmain :: Brain -> IO ()
bfmain brain = let (newbrain, complete) = bfcore brain in do
	putStr $ reverse $ out newbrain
	unless complete $ bfinput newbrain{out=""} >>= bfmain

bfmake :: BrainParse -> BrainParse
bfmake brainParse@(BrainParse [] _ _ _ _) = brainParse
bfmake brainParse@(BrainParse (op:ptail) lastc idx jmptbl code) =
	case op of
		'>' -> acccode 0
		'<' -> acccode 1
		'+' -> acccode 2
		'-' -> acccode 3
		'.' -> bfmake nextParse{code=(4:code)}
		',' -> bfmake nextParse{code=(5:code)}
		'[' -> bfmake nextParse{jmptbl=(idx:jmptbl), code=((6 .|. matchParen 0 ptail (unsafeShiftL (idx+1) 8)):code)}
		']' -> bfmake nextParse{jmptbl=tail jmptbl, code=((7 .|. unsafeShiftL ((head jmptbl)+1) 8):code)}
	where
		nextParse = brainParse{srctail=ptail, lastc=op, idx=idx+1}
		acccode x = bfmake nextParse{idx=if lastc /= op then idx+1 else idx, code=if lastc /= op then ((x+(256::Word32)):code) else (((head code)+(256::Word32)):tail code)}
		matchParen :: Int -> String -> Word32 -> Word32
		matchParen 0 (']':_) n = n
		matchParen x ('[':ptail) n = matchParen (x+1) ptail (n+(256::Word32))
		matchParen x (']':ptail) n = matchParen (x-1) ptail (n+(256::Word32))
		matchParen x (op:ptail) n = matchParen x ptail (if op == '.' || op == ',' || op /= head ptail then (n+(256::Word32)) else n)

uAfromList :: [Word32] -> UA.UArray Int Word32
uAfromList a = UA.listArray (0, (length a-1)) a

bfparse2brain :: BrainParse -> Brain
bfparse2brain brainParse = Brain (uAfromList $ reverse $ code brainParse) 0 0 [0,0..] [0,0..] ""

bfparse :: String -> BrainParse
bfparse str = BrainParse str '\0' 0 [] []

main = getArgs >>= readFile . head >>= (bfmain . bfparse2brain . bfmake . bfparse . (filter $ flip elem $ "><+-.,[]"))