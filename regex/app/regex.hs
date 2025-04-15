import Options.Applicative


-- ==================================== POSIX OPTIONS =============================================

-- Command-line options data type
data Options = NoOp 
  deriving (Show)

-- Parser for command-line options
optionsParser :: Parser Options
optionsParser =
  flag' NoOp
    ( long "no-op"
    <> help "Perform no operation on the regex (just parse and print)" )
  -- <|> will be used to separate different options

optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
    <> progDesc "Parse and process regexes from stdin"
    <> header "regex - a simple regex parser"
  )


-- ============================== MAIN AND HELPER FUNCTIONS =======================================

-- Define a sum type for operation results
data OpResult = Trees [RegexTree] | Strings [String]

-- Define a tree for regex
data RegexTree = Empty                -- Represents the empty set
               | Epsilon              -- Represents the empty string
               | Literal Char         -- Represents any literal character
               | Concat RegexTree RegexTree
               | Union RegexTree RegexTree
               | Star RegexTree
               deriving (Show, Eq)


-- Process a regex string to build a parse tree
buildTree :: String -> RegexTree
buildTree input = head (foldl processChar [] input)
  where
    processChar :: [RegexTree] -> Char -> [RegexTree]
    processChar stack char
      | char == '*' =
          case stack of
            (t:ts) -> 
              -- We need to check for the special case that we read "/*"

              if t == Empty then  -- "If the top of the stack is "/""
                Epsilon : ts      -- Pop the t off by adding epsilon to the tail.

              else                -- t is anything but "/"
                (Star t) : ts     -- Otherwise, just put the star on as normal
            []     -> error "Invalid regex: '*' requires one operand"

      | char == '.' =
          case stack of
            (t2:t1:ts) -> (Concat t1 t2) : ts
            _          -> error "Invalid regex: '.' requires two operands"

      | char == '+' =
          case stack of
            (t2:t1:ts) -> (Union t1 t2) : ts
            _          -> error "Invalid regex: '+' requires two operands"

      | char == '/' = Empty : stack -- Push the empty set onto the stack

      | otherwise = (Literal char) : stack


-- Helper function that exports the parsed Regex tree into a prefix string.
treeToPrefix :: RegexTree -> String
treeToPrefix Empty = "/"
treeToPrefix Epsilon = "*/"
treeToPrefix (Literal c) = [c]
treeToPrefix (Concat t1 t2) = '.' : treeToPrefix t1 ++ treeToPrefix t2
treeToPrefix (Union t1 t2) = '+' : treeToPrefix t1 ++ treeToPrefix t2
treeToPrefix (Star t) = '*' : treeToPrefix t


-- Main
main :: IO ()
main = do

  -- Parse command-line options
  opts <- execParser optsInfo

  -- Read input from stdin until EOF
  input <- getContents

  -- Split the input into lines (while filtering out empty lines)
  -- '.' is the composition funciton, it is used because not expects a bool, but null is a function
  -- so (not null) wouldn't work.
  let linesOfInput = filter (not . null) (lines input)

  -- Build a tree for each
  let trees = map buildTree linesOfInput

  let result = case opts of
    NoOp -> noOpAction trees
    Simplify -> simplifyAction trees

  -- Handle the result
  case result of
    Trees transformedTrees -> do
      -- Print the transformed trees
      mapM_ (putStrLn . treeToPrefix) transformedTrees
    Strings answers -> do
      -- Print the answers
      mapM_ putStrLn answers


-- ======================================== OPERATIONS ============================================

-- No operation action
--  Just call the prefix helper function
noOpAction :: [RegexTree] -> IO ()
noOpAction trees = do

  -- Do nothing to the tree, noOpAction is an identity.

  -- Print the result
  let prefixStrings = map treeToPrefix trees
  mapM_ putStrLn prefixStrings
