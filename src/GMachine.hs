module GMachine where

import Control.Monad.Writer
import Control.Monad.State hiding (withState)

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Function ((&))
import Data.List (mapAccumL)

import Language
import Pretty.Print
import Heap (Heap, Addr, (*!))
import qualified Heap as Heap

runProgram :: String -> String
runProgram = showResults . eval . compile . parseCore

runProgramGhci :: String -> IO ()
runProgramGhci = putStrLn . runProgram

data GmState = GmState { code    :: GmCode
                       , stack   :: GmStack
                       , heap    :: GmHeap
                       , globals :: GmGlobals
                       , stats   :: GmStats
                       }
  deriving (Eq, Show)

applyToCode :: (GmCode -> GmCode) -> GmState -> GmState
applyToCode f state@(GmState { code }) = state { code = f code }

applyToStack :: (GmStack -> GmStack) -> GmState -> GmState
applyToStack f state@(GmState { stack }) = state { stack = f stack}

applyToHeap :: (GmHeap -> GmHeap) -> GmState -> GmState
applyToHeap f state@(GmState { heap }) = state { heap = f heap }

applyToGlobals :: (GmGlobals -> GmGlobals) -> GmState -> GmState
applyToGlobals f state@(GmState { globals }) = state { globals = f globals }

applyToStats :: (GmStats -> GmStats) -> GmState -> GmState
applyToStats f state@(GmState { stats }) = state { stats = f stats }

type GmCode = [Instruction]

data Instruction = Unwind
                 | Pushglobal Name
                 | Pushint Int
                 | Push Int
                 | Mkap
                 | Slide Int
  deriving (Eq, Show, Read)

type GmStack = [Addr]

type GmHeap = Heap Node

data Node = NNum Int
          | NAp Addr Addr
          | NGlobal Int GmCode
  deriving (Eq, Show, Read)

type GmGlobals = Map Name Addr

data GmStats = GmStats Int
  deriving (Eq, Ord, Read, Show)

statInitial :: GmStats
statInitial = GmStats 0

statIncSteps :: GmStats -> GmStats
statIncSteps (GmStats s) = GmStats $ s + 1

statGetSteps :: GmStats -> Int
statGetSteps (GmStats s) = s

-- The evaluator
newtype Eval a = Eval { runEval :: WriterT [GmState] (State GmState) a }
  deriving (Functor, Applicative, Monad, MonadState GmState, MonadWriter [GmState])

withState :: (GmState -> a) -> Eval a
withState f = f <$> get

withCode f = withState (f . code)
withStack f = withState (f . stack)
withHeap f = withState (f . heap)
withGlobals f = withState (f . globals)
withStats f = withState (f . stats)


eval :: GmState -> [GmState]
eval s = runEval evaluate & execWriterT & flip evalState s

evaluate :: Eval ()
evaluate = do
    state <- get
    tell [state]
    if gmFinal state
      then return ()
      else do
        step
        doAdmin
        evaluate

gmFinal :: GmState -> Bool
gmFinal s = code s == []

doAdmin :: Eval ()
doAdmin = modify $ applyToStats statIncSteps

step :: Eval ()
step = do
    (i:is) <- gets code
    modify $ \s -> s { code = is }
    dispatch i

dispatch :: Instruction -> Eval ()
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind

pushglobal :: Name -> Eval ()
pushglobal f = do
    globAddr <- withGlobals $ Map.findWithDefault (error $ "Undeclared global " ++ f) f
    modify $ \s -> s { stack = globAddr : stack s }

pushint :: Int -> Eval ()
pushint n = do
    (heap, a) <- withHeap $ \heap -> Heap.allocate heap (NNum n)
    modify $ \s -> s { stack = a : stack s, heap }

mkap :: Eval ()
mkap = do
    (a1 : a2 : as) <- gets stack
    (heap, a) <- withHeap $ \heap -> Heap.allocate heap (NAp a1 a2)
    modify $ \s -> s { stack = a : as, heap }

push :: Int -> Eval ()
push n = do
    as <- gets stack
    a <- fmap getArg . withHeap $ \heap -> heap *! (as !! (n + 1))
    let stack = a : as
    modify $ \s -> s { stack }
  where getArg (NAp a1 a2) = a2

slide :: Int -> Eval ()
slide n = do
    (a : as) <- gets stack
    modify $ \s -> s { stack = a : drop n as }

unwind :: Eval ()
unwind = do
    (a : as) <- gets stack
    topNode  <- withHeap $ \heap -> heap *! a
    case topNode of
        NNum _ -> return ()
        NAp a1 a2 -> do
            modify $ applyToCode (Unwind :)
            modify $ applyToStack (a1 :)
        NGlobal n code ->
            if length as < n
            then error "Unwinding with too few arguments"
            else modify $ \s -> s { code }


-- The compiler
compile :: CoreProgram -> GmState
compile program = GmState { code = initialCode
                          , stack = []
                          , heap
                          , globals
                          , stats = statInitial
                          }
  where (heap, globals) = buildInitialHeap program
        initialCode = [Pushglobal "main", Unwind]

buildInitialHeap :: [CoreScDefn] -> (GmHeap, GmGlobals)
buildInitialHeap program =
    let (heap, assocs) = mapAccumL allocateSc Heap.empty compiled
    in (heap, Map.fromList assocs)
  where compiled = map compileSc (program ++ preludeDefs) ++ compiledPrimitives

type GmCompiledSc = (Name, Int, GmCode)

compiledPrimitives :: [GmCompiledSc]
compiledPrimitives = []

allocateSc :: GmHeap -> GmCompiledSc -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where (heap', addr) = Heap.allocate heap (NGlobal nargs instns)

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSc
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = [(Name, Int)]

domain :: GmEnvironment -> [Name]
domain e = [ n | (n, _) <- e ]

range :: GmEnvironment -> [Int]
range e = [ i | (_, i) <- e ]

compileR :: GmCompiler
compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]

compileC :: GmCompiler
compileC (EVar v) env
  | v `elem` (domain env) = [Push n]
  | otherwise             = [Pushglobal v]
  where Just n = lookup v env
compileC (ENum n) env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [ (v, m + n) | (v, m) <- env ]


-- Output display
showResults :: [GmState] -> String
showResults states =
    render $ hcat
        [ str "Supercombinator definitions", newline
        , vcat (map (showSC s) (Map.toList $ globals s))
        , newline
        , str "State transitions", newline
        , orderedList (map showState states)
        , newline, newline
        , showStats (last states)
        ]
  where (s:_) = states

showSC :: GmState -> (Name, Addr) -> Doc
showSC s (name, addr) =
    hcat [ str "Code for ", str name, newline
         , showInstructions code, newline
         ]
  where NGlobal _ code = heap s *! addr

showInstructions :: GmCode -> Doc
showInstructions is =
    (str "  Code: ") <> indent (
        (str "{ ") <> indent (
             vcat (map (str . show) is)
        ) <> newline <> str "}"
     )

showState :: GmState -> Doc
showState s = hcat [showStack s, newline, showInstructions (code s), newline]

showStack :: GmState -> Doc
showStack s =
    (str "  Stack: ") <> indent (
        (str "[ ") <> indent (
             vcat (map (showStackItem s) (reverse $ stack s))
        ) <> newline <> str "]"
    )

showStackItem :: GmState -> Addr -> Doc
showStackItem s a = hcat [num a, str ": ", showNode s a (heap s *! a)]

showNode :: GmState -> Addr -> Node -> Doc
showNode _ _ (NNum n) = num n
showNode s a (NGlobal n _) = (str "Global ") <> str v
  where v = head [ n | (n, b) <- (Map.toList $ globals s), b == a]
showNode s a (NAp a1 a2) = hcat [str "Ap ", num a1, str " ", num a2]

showStats :: GmState -> Doc
showStats s = hcat [str "Steps taken = ", num . statGetSteps $ stats s]
