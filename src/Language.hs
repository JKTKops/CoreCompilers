module Language
    ( Expr(..)
    , CoreExpr
    , Name
    , IsRec
    , recursive
    , nonRecursive
    , Alt
    , CoreAlt
    , ScDefn
    , CoreScDefn
    , Program
    , CoreProgram
    , bindersOf
    , rhssOf
    , isAtomicExpr
    , preludeDefs
    , parseCore
    , parseCoreFromFile
    , pprintCore
    )where

import Data.Char (isSpace, isSymbol)
import Text.ParserCombinators.Parsec hiding (spaces, newline)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang (emptyDef)

import Pretty.Print

data Expr a = EVar Name
            | ENum Int
            | EConstr Int Int
            | EAp (Expr a) (Expr a)
            | ELet
                IsRec
                [(a, Expr a)]
                (Expr a)
            | ECase (Expr a) [Alt a]
            | ELam [a] (Expr a)
  deriving (Show, Read, Eq)

type CoreExpr = Expr Name

type Name = String
type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, _) <- defns ]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (_, rhs) <- defns ]

type Alt a = (Int, [a], Expr a)
type CoreAlt = Alt Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

type Program a = [ScDefn a]
type CoreProgram = Program Name


-- Prelude
preludeDefs :: CoreProgram
preludeDefs
  = [ ("I",  ["x"], EVar "x")
    , ("K",  ["x", "y"], EVar "x")
    , ("K1", ["x", "y"], EVar "y")
    , ("S",  ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                  (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f", "g", "x"], EAp (EVar "f")
                                       (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]

-- Parsing
-- TODO: come back through this with Parsec
parseCore :: String -> CoreProgram
parseCore = runCoreParser "<internal>"

parseCoreFromFile :: String -> IO CoreProgram
parseCoreFromFile filename = runCoreParser filename <$> readFile filename

runCoreParser :: String -> String -> CoreProgram
runCoreParser label text = case parse mainParser label text of
    Left err -> error $ show err
    Right prog -> prog
  where mainParser = do
            whiteSpace
            prog <- parseProgram
            eof
            return prog

lexer :: Tok.TokenParser st
lexer = Tok.makeTokenParser def
  where def = Lang.emptyDef
                { Tok.commentLine = "||"
                , Tok.opLetter = oneOf ":!?$%^&*+-./\\<=>@|~"
                , Tok.reservedOpNames =
                    [ "="
                    , "->"
                    , "\\"
                    , "."
                    ]
                , Tok.reservedNames =
                    [ "let"
                    , "letrec"
                    , "in"
                    , "case"
                    , "of"
                    , "Pack"
                    ]
                }

lexeme = Tok.lexeme lexer
whiteSpace = Tok.whiteSpace lexer
parens = Tok.parens lexer
braces = Tok.braces lexer
angles = Tok.angles lexer
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
symbol = Tok.symbol lexer
number = fromInteger <$> Tok.natural lexer
comma = Tok.comma lexer
semiSep = Tok.semiSep lexer
semiSep1 = Tok.semiSep1 lexer

parseProgram :: Parser CoreProgram
parseProgram = semiSep parseSc

parseSc :: Parser CoreScDefn
parseSc = do
    name <- identifier
    args <- many identifier
    reservedOp "="
    body <- parseExpr
    return (name, args, body)

parseExpr :: Parser CoreExpr
parseExpr =  parseLet
         <|> parseCase
         <|> parseLam
         <|> parseExpr1
         <|> parseAExpr

parseLet, parseCase, parseLam, parseAExpr, parseExpr1 :: Parser CoreExpr

parseLet = do
    isrec <- letOrLetrec
    defns <- parseDefns
    reserved "in"
    body <- parseExpr
    return $ ELet isrec defns body

letOrLetrec :: Parser IsRec
letOrLetrec = (reserved "let" >> return False) <|> (reserved "letrec" >> return True)

parseDefns :: Parser [(Name, CoreExpr)]
parseDefns = semiSep1 parseDefn

parseDefn :: Parser (Name, CoreExpr)
parseDefn = do
    name <- identifier
    reservedOp "="
    expr <- parseExpr
    return $ (name, expr)

parseCase = do
    reserved "case"
    expr <- parseExpr
    reserved "of"
    alts <- parseAlts
    return $ ECase expr alts

parseAlts :: Parser [CoreAlt]
parseAlts = semiSep1 parseAlt

parseAlt :: Parser CoreAlt
parseAlt = do
    tag <- angles number
    args <- many identifier
    reservedOp "->"
    body <- parseExpr
    return $ (tag, args, body)

parseLam = do
    reservedOp "\\"
    args <- many1 identifier
    reservedOp "."
    body <- parseExpr
    return $ ELam args body

parseAExpr = lexeme
             (do reserved "Pack"
                 braces $ do
                     tag <- number
                     comma
                     arity <- number
                     return $ EConstr tag arity
          <|> EVar <$> identifier
          <|> ENum <$> number
          <|> parens parseExpr)

-- Pay close attention to the parser used for the RHS - they are not uniform!
-- This implements associativity.
data PartialExpr = NoOp | Op Name CoreExpr
parseExpr1 = lexeme $ do
    e <- parseExpr2
    partial <- (Op <$> symbol "|" <*> parseExpr1) <|> return NoOp
    return $ assembleOp e partial

parseExpr2 :: Parser CoreExpr
parseExpr2 = lexeme $ do
    e <- parseExpr3
    partial <- (Op <$> symbol "&" <*> parseExpr2) <|> return NoOp
    return $ assembleOp e partial

parseExpr3 :: Parser CoreExpr
parseExpr3 = lexeme $ do
    e <- parseExpr4
    partial <- (Op <$> relop <*> parseExpr4) <|> return NoOp
    return $ assembleOp e partial
  where relop = foldr1 (<|>) $ map symbol ["==", "~=", "<", "<=", ">=", ">"]

parseExpr4 :: Parser CoreExpr
parseExpr4 = lexeme $ do
    e <- parseExpr5
    partial <- parseExpr4c
    return $ assembleOp e partial
  where parseExpr4c :: Parser PartialExpr
        parseExpr4c =  (Op <$> symbol "+" <*> parseExpr4)
                   <|> (Op <$> symbol "-" <*> parseExpr5)
                   <|> return NoOp

parseExpr5 :: Parser CoreExpr
parseExpr5 = lexeme $ do
    e <- parseExpr6
    partial <- parseExpr5c
    return $ assembleOp e partial
  where parseExpr5c =  (Op <$> symbol "*" <*> parseExpr5)
                   <|> (Op <$> symbol "/" <*> parseExpr6)
                   <|> return NoOp

parseExpr6 = foldl1 EAp <$> many1 parseAExpr

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (Op op e2) = EAp (EAp (EVar op) e1) e2

-- Pretty Printing (not using libraries for the DIY feel)
pprExpr :: Ppr p => CoreExpr -> p
pprExpr (EVar v) = str v
pprExpr (ENum n) = str $ show n
pprExpr (EConstr tag arity) =
    mconcat [ str "Pack{", num tag, str ", "
            , num arity, str "}"
            ]
pprExpr (EAp (EAp (EVar f) e1) e2) =
    if all isSymbol f
    then pprAExpr e1 <+> str f <+> pprAExpr e2
    else str f <+> pprAExpr e1 <+> pprAExpr e2
pprExpr (EAp e1 e2) = pprExpr e1 <+> pprAExpr e2
pprExpr (ELet isrec defns expr) =
    mconcat [ str keyword, newline
            , str "  ", indent (pprDefns defns), newline
            , str "in ", pprExpr expr
            ]
  where keyword = if isrec then "letrec" else "let"
pprExpr (ECase scrut alts) =
    mconcat [ str "case ", pprExpr scrut, str " of", newline
            , str "  ", indent (pprAlts alts)
            ]
pprExpr (ELam args body) =
    mconcat [ str "(\\", pprArgs args, str ". "
            , indent (pprExpr body), str ")"
            ]

pprDefns :: Ppr p => [(Name, CoreExpr)] -> p
pprDefns defns = mInterleave sep (map pprDefn defns)
  where
    sep = str ";" <> newline

pprDefn :: Ppr p => (Name, CoreExpr) -> p
pprDefn (name, expr) = mconcat [str name, str " = ", indent (pprExpr expr)]

pprAlts :: Ppr p => [CoreAlt] -> p
pprAlts = mInterleave (str ";" <> newline) . map pprAlt

pprAlt :: Ppr p => CoreAlt -> p
pprAlt (tag, args, rhs) =
    mconcat [ str "<", num tag, str ">" <+> pprArgs args
            , str " -> "
            , indent (pprExpr rhs)
            ]

pprArgs :: Ppr p => [Name] -> p
pprArgs = foldl (<+>) nil . map str

pprAExpr :: Ppr p => CoreExpr -> p
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = pprParens $ pprExpr e

pprSc :: Ppr p => CoreScDefn -> p
pprSc (name, args, body) = str name <+> pprArgs args <+> str "=" <+> pprExpr body

pprCore :: Ppr p => CoreProgram -> p
pprCore = mInterleave (str ";" <> newline) . map pprSc

pprintCore :: CoreProgram -> String
pprintCore = render . (pprCore :: CoreProgram -> Doc)
