module Pretty.Print where

class (Eq p, Monoid p) => Ppr p where
    nil     :: p
    nil      = mempty

    append  :: p -> p -> p
    append   = mappend

    str     :: String -> p

    newline :: p
    indent  :: p -> p
    render  :: p -> String

num :: (Ppr p, Num n, Show n) => n -> p
num = strShow

spaces :: Int -> String
spaces = flip replicate ' '

fwNum :: (Ppr p, Show n) => Int -> n -> p
fwNum width n = str (spaces (width - length digits) ++ digits)
  where digits = show n

strShow :: (Ppr p, Show a) => a -> p
strShow = str . show

(<+>) :: (Ppr p, Eq p) => p -> p -> p
p1 <+> p2
  | p1 == nil = p2
  | p2 == nil = p1
  | otherwise = p1 <> str " " <> p2

pprParens :: Ppr p => p -> p
pprParens doc = str "(" <> doc <> str ")"

mInterleave :: Monoid m => m -> [m] -> m
mInterleave sep []     = mempty
mInterleave sep [m]    = m
mInterleave sep (m:ms) = m <> (sep <> mInterleave sep ms)


hcat :: Ppr p => [p] -> p
hcat = mconcat

vcat :: Ppr p => [p] -> p
vcat = mInterleave newline

orderedList :: Ppr p => [p] -> p
orderedList docs = vcat . map mkItem $ zip [1..] docs
  where mkItem (n, doc) = fwNum 4 n <> str ")" <+> indent doc

data Doc = DNil
         | DStr String
         | DAppend Doc Doc
         | DIndent Doc
         | DNewline
  deriving (Eq, Read)

instance Show Doc where show = render

instance Semigroup Doc where (<>) = appendDoc
instance Monoid Doc where
    mempty  = DNil
    mappend = (<>)
instance Ppr Doc where
    str = dStr
    newline = DNewline
    indent = DIndent
    render doc = flatten 0 [(doc, 0)]

dStr :: String -> Doc
dStr s = mInterleave DNewline . map DStr $ lines s

appendDoc :: Doc -> Doc -> Doc
appendDoc DNil doc = doc
appendDoc doc DNil = doc
appendDoc doc1 doc2 = DAppend doc1 doc2

flatten :: Int -> [(Doc, Int)] -> String
flatten _ [] = ""
flatten col ((DNil, _) : docs) = flatten col docs
flatten col ((DStr s, _) : docs) = s ++ flatten (col + length s) docs
flatten col ((DAppend doc1 doc2, indent) : docs) =
    flatten col ((doc1, indent) : (doc2, indent) : docs)
flatten col ((DIndent doc, _) : docs) = flatten col ((doc, col) : docs)
flatten col ((DNewline, indent) : docs) =
   '\n' : spaces indent ++ flatten indent docs
