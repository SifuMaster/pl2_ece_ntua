{-# OPTIONS_GHC -O2 #-}
import Data.Char
import System.IO
import Text.Read
import qualified Data.Map as Map

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

instance Ord Expr where
  compare (Evar s1) (Evar s2) = compare s1 s2
  compare (Evar s1) (Eabs s e) = LT
  compare (Evar s1) (Eapp e1 e2) = LT
  compare (Eabs s1 e1) (Eabs s2 e2) = compare s1 s2 
  compare (Eabs s1 e1) (Eapp e2 e3) = LT 
  compare (Eapp e1 e2) (Eapp e3 e4) = if r /= EQ then r
                                      else compare e2 e4
                                          where r = compare e1 e3 
  compare (Eabs s e) (Evar s1) = compare (Evar s1) (Eabs s e)
  compare (Eapp e1 e2) (Evar s1) = compare (Evar s1) (Eapp e1 e2)
  compare (Eapp e2 e3) (Eabs s1 e1) =  compare (Eabs s1 e1) (Eapp e2 e3)

instance Ord Type where
  compare (Tvar i1) (Tvar i2) = compare i1 i2

typecheck :: Expr -> [(Type, Type)] -> Map.Map Expr Int -> Int -> (Type, Map.Map Expr Int, [(Type, Type)], Int)
typecheck (Evar s) c t n  
                    | Map.notMember (Evar s) t = ((Tvar n), t', c, n+1)
                    | otherwise = (Tvar (t Map.! (Evar s)), t, c, n)
                        where t' = Map.insert (Evar s) n t
typecheck (Eabs s e) c t n = (Tfun (Tvar n) (e_type), t', c', n') 
                        where (e_type, t', c', n') = typecheck e c (Map.insert (Evar s) n t) (n+1)
typecheck (Eapp e1 e2) c t n = ((Tvar n2) , t', c', n')
                        where (e1_type, t1, c1, n1) = typecheck e1 c t n
                              (e2_type, t', c2, n2) = typecheck e2 c1 t1 n1
                              c' = ((e1_type, Tfun e2_type (Tvar n2))):c2
                              n' = n2 + 1


checkIfMember:: Type -> Type -> Bool
checkIfMember (Tvar t1) (Tvar t2) = t1 == t2
checkIfMember (Tvar t1) (Tfun t2 t3) = (checkIfMember (Tvar t1) t2) || (checkIfMember (Tvar t1) t3)

isTvar :: Type -> Bool 
isTvar (Tvar t) = True
isTvar _ = False 

isTfun :: Type -> Bool 
isTfun (Tfun e1 e2) = True
isTfun _ = False

getFstE :: Type -> Type
getFstE (Tfun e1 e2) = e1

getSndE :: Type -> Type
getSndE (Tfun e1 e2) = e2


replaceT :: (Type, Type) -> [(Type, Type)] -> [(Type, Type)]
replaceT (t1, t2) c = map (replace (t1, t2)) c
    where replace :: (Type, Type) -> (Type, Type) -> (Type, Type)
          replace (t1, t2) (t3, t4) = (replace' (t1, t2) t3, replace' (t1, t2) t4)
          replace' :: (Type, Type) -> Type -> Type
          replace' (t1, t2) (Tvar t3)
                  | (t1==(Tvar t3)) = t2
                  | (t1 /= (Tvar t3)) = Tvar t3
          replace' (t1, t2) (Tfun t3 t4) = Tfun (replace' (t1, t2) t3) (replace' (t1, t2) t4)

unify::[(Type, Type)] -> [(Type, Type)]
unify [] = []
unify ((t1, t2):ct)
        | (isTvar t1) && (isTvar t2) && (t1 == t2) = unify(ct)
        | (isTvar t1) && (not (checkIfMember t1 t2)) = (unify c1') ++ [(t1, t2)]
        | (isTvar t2) && (not (checkIfMember t2 t1)) = (unify c2') ++ [(t2, t1)]                                        
        | (isTfun t1) && (isTfun t2) = (unify (ct ++ [(getFstE t1, getFstE t2), (getSndE t1, getSndE t2)]))
        | otherwise = [(Tvar (-1), Tvar (-1))]
                                                  where c1' = replaceT (t1, t2) ct
       
                                                        c2' = replaceT (t2, t1) ct
applySingle :: (Type, Type) -> Type -> Type
applySingle (u1, u2) (Tvar n)  = if (Tvar n) == u1 then u2 else Tvar n
applySingle (u1, u2) (Tfun t1 t2) = Tfun (applySingle (u1, u2) t1) (applySingle (u1, u2) t2)


applyUnifier :: Type -> [(Type, Type)] -> Type
applyUnifier t u = foldr applySingle t u


minifyTypevars :: Type -> Int -> Map.Map Type Type ->  (Type, Int, Map.Map Type Type)
minifyTypevars (Tvar i) n t = if (Map.notMember (Tvar i) t) then (Tvar n, n+1, Map.insert (Tvar i) (Tvar n) t)
                                                            else (t Map.! (Tvar i), n, t)
minifyTypevars (Tfun t1 t2) n t = (Tfun t1' t2', n', t')
                            where (t1', n1, tt) = minifyTypevars t1 n t
                                  (t2', n', t') = minifyTypevars t2 n1 tt


alphaConversion :: Expr -> Int -> Map.Map String String -> (Int, Expr)
alphaConversion (Evar s) n c = if (Map.member s c) then (n, Evar (c Map.! s))
                                                   else (n, Evar s)
alphaConversion (Eabs s e) n c = (n', Eabs s' e')
                              where s' = "x" ++ (show n)
                                    c' = Map.insert s s' c
                                    (n', e') = alphaConversion e (n+1) c'
alphaConversion (Eapp e1 e2) n c = (n', Eapp e1' e2')
                              where (n1, e1') = alphaConversion e1 n c
                                    (n', e2') = alphaConversion e2 n1 c
                                    

get1of4::(Type, Map.Map Expr Int, [(Type, Type)], Int) -> Type
get1of4 (a,_ ,_ , _) = a
  
get3of4::(Type, Map.Map Expr Int, [(Type, Type)], Int) -> [(Type, Type)]
get3of4 (_,_ ,a , _) = a

get1of3::(Type, Int, Map.Map Type Type) -> Type
get1of3 (a, _, _) = a

-- Main program

readOne  =  do  s <- getLine
                let e = read s :: Expr
                let pretty_e = snd $ alphaConversion e 0 Map.empty 
                let typecheck_results = typecheck pretty_e [] Map.empty 0
                let constraints = get3of4 typecheck_results
                let unifier =  unify constraints
                if (not $ null unifier) && (head unifier == (Tvar (-1), Tvar (-1))) then putStr ("type error\n")
                else let etype = get1of4 typecheck_results
                         goodtype = applyUnifier etype unifier
                         bettertype = minifyTypevars goodtype 0 Map.empty
                     in print $ get1of3 bettertype

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                count n readOne
