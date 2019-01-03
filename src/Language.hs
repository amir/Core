module Language where

import Parser

data Expr a
  = EVar Name             -- Variables
  | ENum Int              -- Numbers
  | EConstr Int Int       -- Constructor tag arity
  | EAp (Expr a) (Expr a) -- Applications
  | ELet                  -- Let(rec) expressions
         IsRec            --   boolean with True = recursive,
         [(a, Expr a)]    --   Definitions
         (Expr a)         --   Body of let(rec)
  | ECase                 -- Case expression
          (Expr a)        --   Expression to scrutinise
          [Alter a]       --   Alternatives
  | ELam [a] (Expr a)     -- Lambda abstractions

data Iseq
  = INil
  | IStr String
  | IAppend Iseq Iseq
  | IIndent Iseq
  | INewline
  deriving (Eq, Show)

type Name = String

type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive = True

nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf = map fst

rhssOf :: [(a, b)] -> [b]
rhssOf = map snd

type Alter a = (Int, [a], Expr a)

type ScDefn a = (Name, [a], Expr a)

type Program a = [ScDefn a]

type CoreAlt = Alter Name

type CoreExpr = Expr Name

type CoreScDefn = ScDefn Name

type CoreProgram = Program Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e = False

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave (iStr ";" `iAppend` iNewline) (map pprScDefn prog)

iNil = INil

iStr :: String -> Iseq
iStr = IStr

iNum :: Int -> Iseq
iNum n = IStr $ show n

iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr (spaces (width - length digits) ++ digits)
  where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1 ..] seqs))
  where
    lay_item (n, seq) = iConcat [iFWNum 4 n, iStr ") ", iIndent seq, iNewline]

iNewline = INewline

iIndent seq = seq

iAppend :: Iseq -> Iseq -> Iseq
iAppend = IAppend

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [seq] = seq
iInterleave _ [] = INil
iInterleave i (iseq:iseqs) = iseq `iAppend` i `iAppend` iInterleave i iseqs

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iNum n
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = pprExpr e1 `iAppend` iStr " " `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr) =
  iConcat
    [ iStr keyword
    , iNewline
    , iStr " "
    , iIndent (pprDefns defns)
    , iNewline
    , iStr "in "
    , pprExpr expr
    ]
  where
    keyword
      | not isrec = "let"
      | isrec = "letrec"

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, vars, expr) =
  iStr name `iAppend` iStr " " `iAppend` iInterleave (iStr " ") (map iStr vars) `iAppend`
  iStr sep `iAppend`
  pprExpr expr
  where
    sep =
      if length vars == 0
        then "= "
        else " = "

spaces :: Int -> String
spaces n = replicate n ' '

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2 : e2s

flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""
flatten col ((INil, indent):seqs) = flatten indent seqs
flatten col ((IStr s, indent):seqs) = s ++ flatten col seqs
flatten col ((INewline, indent):seqs) =
  '\n' : spaces indent ++ flatten indent seqs
flatten col ((IIndent seq, indent):seqs) = flatten col ((seq, col) : seqs)
flatten col ((IAppend seq1 seq2, indent):seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x")
  , ("K", ["x", "y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ( "S"
    , ["f", "g", "x"]
    , EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
  , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
  , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]

--syntax :: [Token] -> CoreProgram

--parse :: String -> CoreProgram
--parse = syntax . clex
