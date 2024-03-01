{-# LANGUAGE OverloadedStrings #-}

module Backend.SMTLib where

import Backend.Ast
import Data.List (delete, intercalate)
import Data.Text (pack, unpack, replace)


data SExp =
  SBin SBinOp SExp SExp  | SUn SUnOp SExp | SC String | SCall String [SExp] 
  | SIte SExp SExp SExp

data SBinOp = SAnd | SOr | SEq | SMinus | SPlus | SGt | SLe | SImp | SGeq | SLeq | STimes
data SUnOp = SNot | SUMin

instance Show SExp where
  show (SBin op e1 e2) = "("++(show op)++" "++(show e1)++" "++(show e2)++")"
  show (SUn op e) = "("++(show op)++" "++show e++")"
  show (SC v) = v
  show (SCall f xs) = "("++f++" "++(intercalate " " $ map show xs)++")"
  show (SIte b et ef) = "(ite "++(show b)++" "++(show et)++" "++(show ef)++")"

instance Show SBinOp where
  show SAnd = "and"
  show SOr = "or"
  show SEq = "="
  show SMinus = "-"
  show SPlus = "+"
  show SLe = "<"
  show SGt = ">"
  show SImp = "=>"
  show SLeq = "<="
  show SGeq = ">="
  show STimes = "*"


instance Show SUnOp where
  show SNot = "not"
  show SUMin = "-"

generateSMTLibConst :: Const -> SExp
generateSMTLibConst CTrue = SC "true"
generateSMTLibConst CFalse = SC "false"
generateSMTLibConst (CNum i) = if i >= 0
                                then SC $ show i
                                else SUn SUMin (SC $ show (-i))



mkSExpImp :: SExp -> SExp -> SExp
mkSExpImp e1 e2 = SBin SImp e1 e2

generateSMTLibEPat :: Exp -> (Pattern, Exp) -> SExp
generateSMTLibEPat e (PCon c, e')  = SBin SOr (SUn SNot (SBin SEq (generateSMTLib e) (generateSMTLibConst c))) (generateSMTLib e')
generateSMTLibEPat e (PVar s, e')  =
  SBin SOr (SUn SNot (SBin SEq (generateSMTLib e) (SC s))) (generateSMTLib e')

generateSMTLibEPat _ (Wildcard, e')  = generateSMTLib e'
generateSMTLibEPat _ (_,_) = SC "not supported"


-- will need Stmt (Block) in particular, eventually
generateSMTLib :: Exp -> SExp
generateSMTLib e = case e of
  (Match e' xs) ->  foldr1 (SBin SOr) $ map (generateSMTLibEPat e') xs
  (IfElse be te fe) -> SIte (generateSMTLib be) (generateSMTLib te) (generateSMTLib fe)
  (e1 :== e2) -> SBin SEq (generateSMTLib e1) (generateSMTLib e2)
  (e1 :&& e2) -> SBin SAnd (generateSMTLib e1) (generateSMTLib e2)
  (e1 :|| e2) -> SBin SOr (generateSMTLib e1) (generateSMTLib e2)
  (e1 :> e2) -> SBin SGt (generateSMTLib e1) (generateSMTLib e2)
  (e1 :>= e2) -> SBin SGeq (generateSMTLib e1) (generateSMTLib e2)
  (e1 :< e2 :< e3) -> SBin SAnd (SBin SLe (generateSMTLib e1) (generateSMTLib e2))
                                (SBin SLe (generateSMTLib e2) (generateSMTLib e3))
  (e1 :< e2) -> SBin SLe (generateSMTLib e1) (generateSMTLib e2)
  (e1 :<= e2) -> SBin SLeq (generateSMTLib e1) (generateSMTLib e2)
  (e1 :+ e2) -> SBin SPlus (generateSMTLib e1) (generateSMTLib e2)
  (e1 :* e2) -> SBin STimes (generateSMTLib e1) (generateSMTLib e2)
  (e1 :- e2) -> SBin SMinus (generateSMTLib e1) (generateSMTLib e2)
  (e1 :==> e2) -> mkSExpImp (generateSMTLib e1) (generateSMTLib e2)
  (e1 :<==> e2) -> SBin SAnd
                   (mkSExpImp (generateSMTLib e1) (generateSMTLib e2))
                   (mkSExpImp (generateSMTLib e2) (generateSMTLib e1))
  (e1 :!= e2) -> SUn SNot (SBin SEq (generateSMTLib e1) (generateSMTLib e2))
  (Not ex) -> SUn SNot (generateSMTLib ex)
  (EVar s) -> SC s
  (ECon c) -> generateSMTLibConst c
  (Call f vs) -> SCall f (map generateSMTLib vs)
  (In e1 (ESet xs)) -> foldr (SBin SOr . (SBin SEq (generateSMTLib e1) . generateSMTLib)) (SC "false") xs
  ex -> SC ("not supported: "++show ex)


generateSMTLibSPat :: Bool -> Exp -> (Pattern, Stmt) -> SExp
generateSMTLibSPat b e (PCon c, e')  =
  mkSExpImp (SBin SEq (generateSMTLib e) (generateSMTLibConst c)) (generateSMTLibStmt b e')
generateSMTLibSPat b e (PVar s, e')  =
  mkSExpImp (SBin SEq (generateSMTLib e) (SC s)) (generateSMTLibStmt b e')
generateSMTLibSPat b _ (Wildcard, e')  = generateSMTLibStmt b e'
generateSMTLibSPat _ _ (_,_) = SC "(Pat) Not yet supported"


mkPrime :: [Char] -> [Char]
mkPrime v = v++"!"

mkPrimeIf :: Bool -> [Char] -> [Char]
mkPrimeIf b = if b then mkPrime else id

-- probably will need to have a version of main loop (with var updates)
-- and another version for initialisation things (would become pre?)
generateSMTLibStmt :: Bool -> Stmt -> SExp
generateSMTLibStmt _ (Assign []) = SC "true"
generateSMTLibStmt b (Assign xs) =
  foldr1 (SBin SAnd) $
  map (\(v,e) -> SBin SEq (SC (mkPrimeIf b v)) (generateSMTLib e)) xs
generateSMTLibStmt _ (Block []) = SC "true"
generateSMTLibStmt b (Block xs) = foldr1 (SBin SAnd) $ map (generateSMTLibStmt b) xs
generateSMTLibStmt b (If be te Nothing) =
  mkSExpImp (generateSMTLib be) (generateSMTLibStmt b te)
generateSMTLibStmt b (If be te (Just fe)) = SIte (generateSMTLib be) (generateSMTLibStmt b te) (generateSMTLibStmt b fe)
generateSMTLibStmt b (MatchStmt e' xs) =  foldr1 (SBin SOr) $ map (generateSMTLibSPat b e') xs

generateSMTLibStmt b (While be _ _ s) = SBin SAnd (generateSMTLib be) (generateSMTLibStmt b s)
generateSMTLibStmt _ (Return _) = SC "true" -- SBin SEq (SC "RETURN") (SC "1") -- FIXME
generateSMTLibStmt b (VarDef _ xs) =
  foldr1 (SBin SAnd) $
  map (\(v,_,e) -> SBin SEq (SC (mkPrimeIf b v)) (generateSMTLib e)) xs
generateSMTLibStmt _ (Assert _) = SC "true"

genSMTLibVars :: Stmt -> [String]
genSMTLibVars st = case st of
  (Assign xs) -> map fst xs
  (VarDef _ xs) -> map (\(x,_,_) -> x) xs
  (Block xs) -> concatMap genSMTLibVars xs
  _ -> []

genSMTLibInit :: Stmt -> SExp
genSMTLibInit st = case st of
  (Assign _) -> generateSMTLibStmt False st
  (VarDef _ _) -> generateSMTLibStmt False st
  (Block []) -> SC "true"
  (Block ((While _ _ _ _):_)) -> SC "true"
  (Block (x:xs)) -> SBin SAnd (generateSMTLibStmt False x) (genSMTLibInit (Block xs))
  (Assert e) -> generateSMTLib e
  _ -> SC "true"

genSMTLibLoop :: Stmt -> SExp
genSMTLibLoop st = case st of
  (Block []) -> SC "true"
  (Block (w@(While _ _ _ _):_)) -> generateSMTLibStmt True w
  (Block (_:xs)) -> genSMTLibLoop (Block xs)
  _ -> SC "true"


genSMTLibLoopInvariants :: Stmt -> [SExp]
genSMTLibLoopInvariants st = case st of
  (Block []) -> [SC "true"]
  (Block ((While _ invs _ _):_)) -> map generateSMTLib invs
  (Block (_:xs)) -> genSMTLibLoopInvariants (Block xs)
  _ -> [SC "true"]

getMainMethod :: Program -> Stmt
getMainMethod (Program xs) = getMainMethodBody $ last xs
  where getMainMethodBody (MDecl m) = methodBody m

getPostCondition :: Program -> [Exp]
getPostCondition (Program xs) = helper $ last xs
  where helper (MDecl m) = ensures $ methodHoare m

getMethodParams :: Program -> [(String, Type)]
getMethodParams (Program xs) = helper $ last xs
  where helper (MDecl m) = params $ methodHoare m


-- to check
-- Pre => Inv
-- not termination && Inv(x) && Trans ==> Inv(x!)
-- termination && inv(x) => post
genSMTLibHoareTriple :: Program -> String
genSMTLibHoareTriple prog =
  let st = getMainMethod prog
      postc = foldr (SBin SAnd) (SC "true") (map generateSMTLib $ getPostCondition prog)
      vars = delete "sch" $ (map fst $ getMethodParams prog)++(genSMTLibVars st)
      inits = genSMTLibInit st
      loop = genSMTLibLoop st
      inv = foldr (SBin SAnd) (SC "true") $ genSMTLibLoopInvariants st
      varsig b = unwords $ map (\x -> "("++mkPrimeIf b x++" Int)") vars
      sorts = unwords $ map (const "Int") vars
      varsNames b = unwords $ map (mkPrimeIf b) vars
      stddcl = ("(synth-fun sch ((x Int)) Int)" :: String) -- uninterpreted function decl as allowed in sygus
      invSig = "(declare-fun inv_fun ("++sorts++") Bool)"
      invBody = "(assert (forall ("++varsig False++") (= (inv_fun "++varsNames False++") "++(show inv)++") ))"

      vardecl = intercalate "\n" $ map (\x -> "(declare-const "++x++" Int)") vars
      preSig = "(declare-fun pre_fun ("++sorts++") Bool)"
      preBody = "(assert (forall ("++varsig False++") (= (pre_fun "++varsNames False++") "++(show inits)++") ))"
    

      transdecl = "(define-fun trans_fun ("++(varsig False)++" "++(varsig True)++") Bool \n "++(show loop)++")"
      postdecl = "(define-fun post_fun ("++(varsig False)++") Bool "++(show postc)++")"
      stdhead = "(set-logic ALL)" -- should choose a logic eventually ?
      preInv = "(assert (=> (pre_fun "++varsNames False++") (inv_fun "++varsNames False++")))"
      stdfoot = "(check-sat)"
      
  in unpack $
     replace "T0" "2" $
     replace "T1" "3" $
     replace "T2" "5" $
     pack $
     intercalate "\n\n" [stdhead, preSig, preBody, invSig, invBody, vardecl, preInv, stdfoot]


genSMTLibInvSynth :: Program -> String
genSMTLibInvSynth prog =
  let st = getMainMethod prog
      postc = foldr (SBin SAnd) (SC "true") (map generateSMTLib $ getPostCondition prog)
      vars = delete "sch" $ (map fst $ getMethodParams prog)++(genSMTLibVars st)
      inits = genSMTLibInit st
      loop = genSMTLibLoop st
      varsig b = intercalate " " $ map (\x -> "("++(mkPrimeIf b x)++" Int)") vars
      stddcl = "(synth-fun sch ((x Int)) Int)" -- uninterpreted function decl as allowed in sygus
      -- vardecl = intercalate "\n" $ map (\x -> "(declare-const "++x++" Int)") vars
      predecl = "(define-fun pre_fun ("++(varsig False)++") Bool "++(show inits) ++")"
      invdecl = "(synth-inv inv_fun ("++(varsig False)++"))"
      cstsch = "(constraint (forall ((x Int)) (and (>= (sch x) 0) (< (sch x) 3))))"
      transdecl = "(define-fun trans_fun ("++(varsig False)++" "++(varsig True)++") Bool \n "++(show loop)++")"
      postdecl = "(define-fun post_fun ("++(varsig False)++") Bool "++(show postc)++")"
      stdhead = "(set-logic ALL)" -- should choose a logic eventually ?
      stdfoot = "(inv-constraint inv_fun pre_fun trans_fun post_fun) \n (check-synth)"
  in unpack $
     replace "T0" "2" $ 
     replace "T1" "3" $
     replace "T2" "5" $
     pack $
     intercalate "\n\n" [stdhead, invdecl, stddcl, cstsch, predecl, transdecl, postdecl, stdfoot]