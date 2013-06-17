-- Renato Neves, email:nevrenato@gmail.com
module X
where 

import HyLo.InputFile
import HyLo.Formula
import HyLo.Signature.String (NomSymbol(..), PropSymbol(..), RelSymbol(..))

import Data.Set hiding (foldr)
import Data.List (intersperse)

type SymSet = (Set String, Set String, Set String) 
triEmpty = (empty, empty, empty)

main :: FilePath -> IO ()
main f = (convertToCASL f) >>= (writeFile (f++"c"))

convertToCASL :: FilePath -> IO String
convertToCASL a = work a >>= (return . toCASLConverter)
	where work = parseWithIO . readFile

parseWithIO :: IO String -> IO InputFile
parseWithIO s = s >>= (return . parse)

-- The resulting formulas are negated, since
-- we care not about sat or unsat, but valid
-- or not. If we negate a formula and
-- get unsat, then the result is valid. On the other 
-- hand if is
-- sat, then is not valid (disproved).
toCASLConverter :: InputFile -> String
toCASLConverter i = 
	(addCASLHeader (collectSymbols i)) $ 
		(toProvetag . neg . connectByConj) $ 
		(foldr (\a b -> (wrapInPar . converter) a : b) [] i)
	where 
		connectByConj = unlines . (intersperse ("/\\"))	
		neg = ((++) ".not ") . wrapInPar
		toProvetag a =  (++) a "%implied %(toprove)%"

converter :: (Formula NomSymbol PropSymbol RelSymbol) -> String
converter f = case f of 
	Top -> "true"
	Bot -> "false"
	(Prop p) -> ppp p
	(Nom n) -> "Here "++(ppn n)
	(Neg g) -> "not "++(convWrap g)
	(g :&: g') -> wrap (convWrap g) "/\\" (convWrap g')
	(g :|: g') -> wrap (convWrap g) "\\/" (convWrap g')
	(g :-->: g') -> wrap (convWrap g) "=> " (convWrap g')
	(g :<-->: g') -> wrap (convWrap g) "<=>" (convWrap g')
	(Diam r g) -> wrapInDia (ppm r)++(convWrap g)	
	(Box r g) -> wrapInBox (ppm r)++(convWrap g)
	(At n g) -> wrap "@" (ppn n) (convWrap g)
	--f (A g) s = f g s
	--f (E g) s = f g s
	--f (Down n g) s = f g s
	where
		--pretty printer
		ppp (PropSymbol p) = p
		ppn (NomSymbol n) = n
		ppm (RelSymbol m) = m	
		convWrap = wrapInPar . converter

cASLHeader :: SymSet -> String
cASLHeader (a,b,c) =
	"%%This file was automatic generated, by an input in hylolib format"
	++"\n"
	++"logic Hybrid"
	++"\n"
	++"spec X =\n"
	++preds
	++noms
	++mods
	where
		f = concat . (intersperse ",") . elems
		preds = if (a == empty) then ""
				else "preds "++(f a)++":()\n"
		noms = if (b == empty) then ""
				else "nominals "++(f b)++"\n"
		mods = if (c == empty) then ""
				else "modalities "++(f c)++"\n"


addCASLHeader :: SymSet -> String -> String
addCASLHeader x = (cASLHeader x ++) 
-- the function below collects symbols from a list
-- of formulas, returning a triple of sets of type
-- (Set prop, Set noms, Set mods)
collectSymbols :: InputFile -> SymSet
collectSymbols = foldr f triEmpty
	where
		--pretty print
		ppp (PropSymbol p) = p
		ppn (NomSymbol n) = n
		ppm (RelSymbol m) = m
		-- inserter
		addProp (PropSymbol p) (a,b,c) = (insert p a, b,c )
		addNom (NomSymbol n) (a,b,c) = (a, insert n b, c)
		addMod (RelSymbol m) (a,b,c) = (a, b, insert m c)
		--symbol collector
		f Top s = s
		f Bot s = s
		f (Prop p) s = addProp p s
		f (Nom n) s = addNom n s
		f (Neg g) s = f g s
		f (g :&: g') s = tripUnion (f g s) (f g' triEmpty)
		f (g :|: g') s = tripUnion (f g s) (f g' triEmpty)
		f (g :-->: g') s = tripUnion (f g s) (f g' triEmpty)
		f (g :<-->: g') s = tripUnion (f g s) (f g' triEmpty)
		f (Diam r g) s = f g (addMod r s)
		f (Box r g) s = f g (addMod r s)
		f (At n g) s = f g (addNom n s)
		f (A g) s = f g s
		f (E g) s = f g s
		f (Down n g) s = f g s

wrap :: String -> String -> String -> String
wrap w s w'  = w++" "++s++" "++w'

wrapInPar :: String -> String
wrapInPar s = wrap "(" s ")" 

wrapInDia :: String -> String
wrapInDia s = wrap "<" s ">"

wrapInBox :: String -> String
wrapInBox s = wrap "[" s "]"

tripUnion :: SymSet -> SymSet -> SymSet
tripUnion (a,b,c) (a',b',c') = (union a a', union b b', union c c')