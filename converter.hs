-- Renato Neves, email:nevrenato@gmail.com
-- Here lies two converters. One from HTab to HCasl, and another from HTab to HyLoRes

module X
where 

import HyLo.InputFile
import HyLo.Formula
import HyLo.Signature.String (NomSymbol(..), PropSymbol(..), RelSymbol(..))

import Data.Set hiding (foldr)
import Data.List (intersperse)

type SymSet = (Set String, Set String, Set String) 
triEmpty = (empty, empty, empty)

-- 1s part of this system, namely the converter to HCasl
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
	(A g) -> "! y "++("@ y "++(convWrap g)) 
	(E g) -> "? y "++("@ y "++(convWrap g)) 
	(Down n g) -> "? "++(ppn n)++" "++
			(wrapInPar $ "Here "++(ppn n)++" /\\ "++(convWrap g))
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
		f (Down n g) s = let (a,b,c) = f g s in (a, delete (ppn n) b, c)
		-- one must use delete, so that nominal variables 
		-- are not declared in the signature
-------------------------
-- 2nd part of this system. Namely the converter to HyLoRes

main_Res :: FilePath -> IO ()
main_Res f = (convertToRes f) >>= (writeFile (f++"r"))


convertToRes :: FilePath -> IO String
convertToRes a = work a >>= (return . toResConverter)
	where work = parseWithIO . readFile

toResConverter :: InputFile -> String
toResConverter i =
	(addResHeader . connect) $ 
		foldr (\a b -> converterRes a : b) [] i
	where
		addResHeader s = "begin\n"++s++"\nend"
		connect = unlines . (intersperse ";")

converterRes :: (Formula NomSymbol PropSymbol RelSymbol) -> String
converterRes f = case f of 
	Top -> "true"
	Bot -> "false"
	(Prop p) -> ppp p
	(Nom n) -> ppn n
	(Neg g) -> "!"++(convWrap g)
	(g :&: g') -> wrap (convWrap g) "&" (convWrap g')
	(g :|: g') -> wrap (convWrap g) "|" (convWrap g')
	(g :-->: g') -> wrap (convWrap g) "-->" (convWrap g')
	(g :<-->: g') -> wrap (convWrap g) "<-->" (convWrap g')
	(Diam r g) -> wrapInDia (ppm r)++(convWrap g)	
	(Box r g) -> wrapInBox (ppm r)++(convWrap g)
	(At n g) -> (ppn n)++": "++(convWrap g)
	(A g) -> "A"++convWrap g 
	(E g) -> "E"++convWrap g 
	(Down n g) -> (wrapInPar . ("down " ++) . wrapInPar . ((("x" ++) $ tail $ ppn n) ++)) (convWrap (rep (ppn n) g))
	where
		--pretty printer
		ppp (PropSymbol p) = p
		ppn (NomSymbol n) = n
		ppm (RelSymbol m) = m	
		convWrap = wrapInPar . converterRes
		

-- The following function, replaces nominals variables in the format "nX" 
-- to "xX", thereby conforming to HyLoRes.
rep s f' = case f' of
	Top -> Top
	Bot -> Bot
	(Prop p) -> Prop p
	Nom n ->  (Nom. NomSymbol) $ fNom s $ ppn n 
	Neg g -> Neg g
	(g :&: g') -> (rep s g) :&: (rep s g')
	(g :|: g') -> (rep s g) :|: (rep s g')
	(g :-->: g') -> (rep s g) :-->: (rep s g')
	(g :<-->: g') -> (rep s g) :<-->: (rep s g')
	(Diam r g) -> Diam r (rep s g)
	(Box r g) -> Box r (rep s g)
	(At n g) -> At (NomSymbol $ fNom s $ ppn n) (rep s g)
	(A g) -> A (rep s g)
	(E g) -> E (rep s g)
	(Down n g) -> Down n (rep s g)
	where 
	--pretty printer
	ppn (NomSymbol n) = n
	fNom s n = if (n == s) 
				then ("x" ++) $ tail n 	
				else n 

------------
-- Auxiliar functions
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