-- Renato Neves, email:nevrenato@gmail.com
module X
where 

import HyLo.InputFile
import HyLo.Formula
import HyLo.Signature.String (NomSymbol(..), PropSymbol(..), RelSymbol(..))

import Data.Set hiding (foldr)
import Data.List (intersperse)

main :: FilePath -> IO ()
main f = (convertToCASL f) >>= (writeFile (f++"c"))

convertToCASL :: FilePath -> IO String
convertToCASL a = work a >>= (return . toCASLConverter)
	where work = parseWithIO . readFile

parseWithIO :: IO String -> IO InputFile
parseWithIO s = s >>= (return . parse)

toCASLConverter :: InputFile -> String
toCASLConverter i = 
	(addCASLHeader (collectSymbols i)) $ unlines $ (foldr (\a b -> f a : b) [] i)
	where f _ = ""
-----------------------
type SymSet = (Set String, Set String, Set String) 

cASLHeader :: SymSet -> String
cASLHeader (a,b,c) =
	"%%This file was automatic generated, by an input in hylolib format"
	++"\n"
	++"logic Hybrid"
	++"\n"
	++"spec X =\n"
	++"preds "++(f a)++":()\n"
	++"nominals "++(f b)++"\n"
	++"modalities "++(f c)++"\n"
	where
		f = concat . (intersperse ",") . elems



addCASLHeader :: SymSet -> String -> String
addCASLHeader x = (cASLHeader x ++) 
-- the function below collects symbols from a list
-- of formulas, returning a triple of sets of type
-- (Set prop, Set noms, Set mods)
collectSymbols :: InputFile -> SymSet
collectSymbols = foldr f emptytr
	where
		emptytr = (empty, empty, empty) 
		--pretty print
		ppp (PropSymbol p) = p
		ppn (NomSymbol n) = n
		ppm (RelSymbol m) = m
		--symbol collector
		f Top s = s
		f Bot s = s
		f (Prop p) (a,b,c) = (insert (ppp p) a, b, c)
		f (Nom n) (a,b,c) = (a, insert (ppn n) b, c)
		f (Neg g) s = f g s
		f (g :&: g') s = uniTrip (f g s) (f g' emptytr)
		f (g :|: g') s = uniTrip (f g s) (f g' emptytr)
		f (g :-->: g') s = uniTrip (f g s) (f g' emptytr)
		f (g :<-->: g') s = uniTrip (f g s) (f g' emptytr)
		f (Diam r g) (a,b,c) = f g (a,b,insert (ppm r) c)
		f (Box r g) (a,b,c) = f g (a,b,insert (ppm r) c)
		f (At n g) (a,b,c) = f g (a, insert (ppn n) b,c)
		f (A g) s = f g s
		f (E g) s = f g s
		f (Down n g) s = f g s

uniTrip :: SymSet -> SymSet -> SymSet
uniTrip (a,b,c) (a',b',c') = (union a a', union b b', union c c')