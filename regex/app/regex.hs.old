import Options.Applicative


-- ==================================== POSIX OPTIONS =============================================

-- Command-line options data type
data Options = Tree
             | NoOp
             | Simplify
  deriving (Show)

-- Parser for command-line options
optionsParser :: Parser Options
optionsParser =
    flag' Tree
    ( long "tree"
    <> help "Print the parsed regex tree for debugging" )
  <|> flag' NoOp
    ( long "no-op"
    <> help "Perform no operation on the regex (just parse and print)" )
  <|> flag' Simplify
    ( long "simplify"
    <> help "Simplify the regex" )

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
                Star t : ts     -- Otherwise, just put the star on as normal
            []     -> error "Invalid regex: '*' requires one operand"

      | char == '.' =
          case stack of
            (t2:t1:ts) -> Concat t1 t2 : ts
            _          -> error "Invalid regex: '.' requires two operands"

      | char == '+' =
          case stack of
            (t2:t1:ts) -> Union t1 t2 : ts
            _          -> error "Invalid regex: '+' requires two operands"

      | char == '/' = Empty : stack -- Push the empty set onto the stack

      | otherwise = Literal char : stack


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
    let linesOfInput = filter (not . null) (lines input)

    -- Build a tree for each line
    let trees = map buildTree linesOfInput

    -- Apply the appropriate action based on the option
    let result = case opts of
            Tree -> Strings (map show trees)  -- Convert trees to strings for direct printing
            NoOp -> Trees (noOpAction trees)
            Simplify -> Trees (simplifyAction trees)

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
noOpAction :: [RegexTree] -> [RegexTree]
noOpAction trees = trees   -- Do nothing to the tree, noOpAction is an identity.

-- Simplify action
simplifyAction :: [RegexTree] -> [RegexTree]
simplifyAction = map simplifyTree
  where
    simplifyTree :: RegexTree -> RegexTree

    -- Double star case
    simplifyTree (Star t@(Star _)) = simplifyTree t             -- t** = t*

    -- (s or e)* case
    simplifyTree (Star (Union t1 t2)) =
      let t1' = simplifyTree t1
          t2' = simplifyTree t2
      in case (t1', t2') of
           (Epsilon, _) -> simplifyTree (Star t2')                  -- (e + s)* = s*
           (_, Epsilon) -> simplifyTree (Star t1')                  -- (s + e)* = s*
           _ -> Star (Union t1' t2')                                -- Otherwise, keep the structure

    -- Star of empty set
    simplifyTree (Star Empty) = Epsilon                         -- /* = e

    -- Star of epsilon
    simplifyTree (Star Epsilon) = Epsilon                      -- e* = e

    -- Simplify other stars
    simplifyTree (Star t) =
      let simplifiedT = simplifyTree t
      in case simplifiedT of
           Empty -> Epsilon                                     -- /* = e
           Epsilon -> Epsilon                                   -- e* = e
           _ -> Star simplifiedT                                -- Otherwise, keep the star

    -- Union cases
    simplifyTree (Union t1 t2)
      | t1 == Empty && t2 == Empty = Empty                      -- / + / = /
      | t1 == Empty = simplifyTree t2                           -- / + t = t
      | t2 == Empty = simplifyTree t1                           -- t + / = t
      | t1 == Epsilon && t2 == Epsilon = Epsilon                -- e + e = e
      | t1 == Epsilon = Union Epsilon (simplifyTree t2)         -- e + t = t
      | t2 == Epsilon = Union (simplifyTree t1) Epsilon         -- t + e = t
      | otherwise =
          let t1' = simplifyTree t1
              t2' = simplifyTree t2
          in case (t1', t2') of
                (Empty, Empty) -> Empty                 -- / + / = /
                (Empty, _)   -> t2'                     -- / + t = t
                (_, Empty)   -> t1'                     -- t + / = t
                (Epsilon, Epsilon) -> Epsilon           -- e + e = e
                _           -> Union t1' t2'            -- (t + s)' = t' + s'  

    -- Concat cases
    simplifyTree (Concat t1 t2)
      | t1 == Empty || t2 == Empty = Empty                      -- / . t = /, t . / = /
      | t1 == Epsilon = simplifyTree t2                         -- e . t = t
      | t2 == Epsilon = simplifyTree t1                         -- t . e = t
      | otherwise =
          let t1' = simplifyTree t1
              t2' = simplifyTree t2
          in case (t1', t2') of
                (Empty, _)   -> Empty                             -- / . t = /
                (_, Empty)   -> Empty                             -- t . / = /
                (Epsilon, _) -> t2'                               -- e . t = t
                (_, Epsilon) -> t1'                               -- t . e = t
                _           -> Concat t1' t2'                    -- (s . t)' = s' . t'

    -- Base cases
    simplifyTree (Literal c)  = Literal c                       -- base case
    simplifyTree Epsilon      = Epsilon                         -- base case
    simplifyTree Empty        = Empty                           -- base case
