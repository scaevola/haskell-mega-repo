module Futurice.App.MegaRepoTool.Estimator (estimator) where

import Data.Char        (digitToInt)
import Data.Either      (partitionEithers)
import Data.List        (foldl')
import Futurice.Prelude
import Prelude ()
import Text.Printf      (printf)

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Trifecta                as Tri

estimator :: FilePath -> IO ()
estimator fp = do
    result <- Tri.parseFromFileEx (topParser <* Tri.eof) fp
    case result of
        Tri.Failure ei   -> print (Tri._errDoc ei)
        Tri.Success node -> do
            let node' = estimateToDistr <$> node
            print (ppNode "" node')

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Node a = Node
    { _nodeName     :: !String
    , _nodeTasks    :: [Task a]
    , _nodeChildren :: [Node a]
    }
  deriving (Show, Functor, Foldable, Traversable)

data Task a = Task
    { _taskName  :: !String
    , _taskValue :: a
    }
  deriving (Show, Functor, Foldable, Traversable)

data Estimate = Estimate
    { _taskBest    :: !Int
    , _taskNominal :: !Int
    , _taskWorst   :: !Int
    }
  deriving Show

data Distr = Distr
    { _distrMean   :: !Double
    , _distrStddev :: !Double
    }
  deriving Show

instance Semigroup Distr where
    Distr a b <> Distr a' b' = Distr (a +  a') (b + b')

instance Monoid Distr where
    mempty = Distr 0 0
    mappend = (<>)

estimateToDistr :: Estimate -> Distr
estimateToDistr (Estimate b n w) = Distr
    (fromIntegral (b + 4 * n + w) / 6)
    (sq (fromIntegral (abs (b - w)) / 6))
  where
    sq x = x * x

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

ppNode :: String -> Node Distr -> PP.Doc
ppNode pfx n@(Node title ts ns) = titleDoc
    PP.<$> PP.vsep (ppTask <$> ts)
    PP.<$> PP.vsep (ppNode ('#' : pfx) <$> ns)
  where
    pfxDoc
        | null pfx    = PP.magenta $ PP.string ">"
        | pfx == "#"  = PP.cyan $ PP.string pfx
        | pfx == "##" = PP.green $ PP.string pfx
        | otherwise   = PP.string pfx

    titleDoc = pfxDoc PP.<+> ppDistr (fold n) PP.<+> PP.bold (PP.string title)

ppTask :: Task Distr -> PP.Doc
ppTask (Task title d) = PP.string "-"
    PP.<+> ppDistr d
    PP.<+> PP.string title

ppDistr :: Distr ->  PP.Doc
ppDistr (Distr u sd) =
    PP.string (printf "%.1f" u)
    PP.<> PP.char '±'
    PP.<> PP.string (printf "%.1f" $ 2 * var)
  where
    var = sqrt sd

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

topParser :: Tri.Parser (Node Estimate)
topParser = do
    Tri.skipMany (commentParser <|> nlParser)
    sections <- many (nodeParser "#")
    pure (Node "Top" [] sections)

nodeParser :: String -> Tri.Parser (Node Estimate)
nodeParser pfx = do
    _ <- Tri.string pfx
    spParser
    title <- restOfLine
    (tasks, nodes) <- partitionEithers <$>
        many (Left <$> taskParser <|> Right <$> nodeParser ('#' : pfx))
    pure (Node title tasks nodes)

taskParser :: Tri.Parser (Task Estimate)
taskParser = do
    _ <- Tri.char '-'
    spParser
    b <- intParser
    _ <- Tri.char ':'
    n <- intParser
    _ <- Tri.char ':'
    w <- intParser
    spParser
    title <- restOfLine
    pure (Task title (Estimate b n w))

intParser :: Tri.Parser Int
intParser = foldl' (\x y -> x * 10 + y) 0 <$> some (digitToInt <$> Tri.digit)

-- | Some spaces, not newlines.
spParser :: Tri.Parser ()
spParser = Tri.skipSome (Tri.oneOf " \t")

nlParser :: Tri.Parser ()
nlParser = void (Tri.char '\n')

restOfLine :: Tri.Parser String
restOfLine = many (Tri.noneOf "\n") <* Tri.skipSome nlParser

commentParser :: Tri.Parser ()
commentParser = void (Tri.char '>' >> restOfLine)
