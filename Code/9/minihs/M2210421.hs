-- name: Tran Cong Thanh
-- id: 2210421
-- acknowledgements:

import TRS
import Parser
import System.Environment

substitute :: Term -> Substitution -> Term
subsitute t [] = t
substitute (Var x) sigma
  | Just t <- lookup x sigma = t
  | otherwise                = Var x
substitute (Con c) sigma     = Con c
substitute (App t1 t2) sigma = App (substitute t1 sigma) (substitute t2 sigma)

match :: Term -> Term -> Maybe Substitution
match t1 t2 | t1 == t2 = Just []
match (Var x) t     = Just [(x, t)]
match (App t1 t2) (App t3 t4)
  | Just ls <- (match t1 t3), Just rs <- (match t2 t4) = Just $ ls ++ rs
match _ _ = Nothing

rewriteAtRoot :: TRS -> Term -> Maybe Term
rewriteAtRoot [] t = Nothing
rewriteAtRoot ((t1, t2) : trs) t
  | Just sub <- match t1 t = Just $ substitute t2 sub
  | otherwise              = rewriteAtRoot trs t

rewrite :: TRS -> Term -> Maybe Term
rewrite trs (Var x)
  | Just y <- rewriteAtRoot trs (Var x) = Just $ y
  | otherwise                           = Just $ Var x
rewrite trs (Con c)
  | c == "main" = rewriteAtRoot trs (Con "main")
  | otherwise   = Just $ Con c
rewrite trs (App t1 t2)
  | Just t <- rewriteAtRoot trs (App ls rs) = Just t
  | otherwise                               = Just $ App ls rs
  where Just ls = (rewrite trs t1)
        Just rs = (rewrite trs t2)

nf1 :: TRS -> Term -> Term
nf1 trs t
  | Just t' <- rewrite trs t, t /= t' = nf1 trs t'
  | otherwise                         = t


data MarkedTerm = MApp MarkedTerm MarkedTerm | MCon String | NF Term

substitute2 :: Term -> Substitution -> MarkedTerm
substitute2 (Var x) sigma
  | Just t <- lookup x sigma = NF t
  | otherwise                = NF $ Var x
substitute2 (Con c) sigma     = MCon c
substitute2 (App t1 t2) sigma = MApp (substitute2 t1 sigma) (substitute2 t2 sigma)

rewriteAtRoot2 :: TRS -> Term -> MarkedTerm
rewriteAtRoot2 [] t = NF t
rewriteAtRoot2 ((t1, t2) : trs) t
  | Just sub <- match t1 t = substitute2 t2 sub
  | otherwise              = rewriteAtRoot2 trs t

rewrite2 :: TRS -> MarkedTerm -> MarkedTerm
rewrite2 trs (NF t) = NF t
rewrite2 trs (MCon c)
  | c == "main" = rewriteAtRoot2 trs (Con "main")
  | otherwise   = NF $ Con c
rewrite2 trs (MApp t1 t2)
  | NF lt <- ls, NF rt <- rs = rewriteAtRoot2 trs (App lt rt)
  | otherwise = MApp (rewrite2 trs ls) (rewrite2 trs rs)
  where ls = (rewrite2 trs t1)
        rs = (rewrite2 trs t2)

nf2 :: TRS -> MarkedTerm -> Term
nf2 trs t
  | NF n <- t' = n
  | otherwise = nf2 trs t'
  where t' = rewrite2 trs t


main :: IO ()
main = do
  file : _ <- getArgs
  result <- readTRSFile file
  case result of
    Left e    -> print e
    Right trs -> do
      -- putStrLn (show (nf1 trs (Con "main")))
      putStrLn (show (nf2 trs (MCon "main")))
