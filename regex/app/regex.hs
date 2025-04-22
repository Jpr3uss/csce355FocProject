import Options.Applicative

-- ==================================== POSIX OPTIONS =============================================

-- Command-line options data type
data Options = Tree
             | NoOp
             | Simplify
             | Empty
             | HasEpsilon
             | HasNonEpsilon
             | Uses String
             | NotUsing String
             | Infinite
             | StartsWith String
             | Reverse
             | EndsWith String
             | Prefixes
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
    <|> flag' Empty
        ( long "empty"
        <> help "Check if the regex language is empty" )
    <|> flag' HasEpsilon
        ( long "has-epsilon"
        <> help "Check if the regex language contains epsilon" )
    <|> flag' HasNonEpsilon
        ( long "has-nonepsilon"
        <> help "Check if the regex language contains some non-empty string" )
    <|> Uses <$> strOption
        ( long "uses"
        <> metavar "STRING"
        <> help "Check if the regex language contains a string that contains one of the given characters" )
    <|> NotUsing <$> strOption
        ( long "not-using"
        <> metavar "STRING"
        <> help "Output a new regex whos language doesn't include a in any string" )
    <|> flag' Infinite
        ( long "infinite"
        <> help "Check if the regex language is infinite" )
    <|> StartsWith <$> strOption
        ( long "starts-with"
        <> metavar "STRING"
        <> help "Check if the regex language contains a string that starts with one of the given characters" )
    <|> flag' Reverse
        ( long "reverse"
        <> help "Reverse the regex, the trees will NOT be simplified" )
    <|> EndsWith <$> strOption
        ( long "ends-with"
        <> metavar "STRING"
        <> help "Check if the regex language contains a string that ends with one of the given characters"
        )
    <|> flag' Prefixes
        ( long "prefixes"
        <> help "Output a new regex whos language denotes all prefixes of strings in the original language" )

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
data RegexTree = Null                -- Represents the empty set
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

              if t == Null then  -- "If the top of the stack is "/""
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

      | char == '/' = Null : stack -- Push the empty set onto the stack

      | otherwise = Literal char : stack


-- Helper function that exports the parsed Regex tree into a prefix string.
treeToPrefix :: RegexTree -> String
treeToPrefix Null = "/"
treeToPrefix Epsilon = "*/"
treeToPrefix (Literal c) = [c]
treeToPrefix (Concat t1 t2) = '.' : treeToPrefix t1 ++ treeToPrefix t2
treeToPrefix (Union t1 t2) = '+' : treeToPrefix t1 ++ treeToPrefix t2
treeToPrefix (Star t) = '*' : treeToPrefix t


-- Helper function that maps true and false to "yes" and "no" respectively
boolToString :: Bool -> String
boolToString True  = "yes"
boolToString False = "no"


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
        -- Convert trees to strings for direct printing
            Tree            -> Strings  (map show trees)

        -- Do nothing, just parse and print
            NoOp            -> Trees    (noOpAction trees)

        -- Simplify the regex
            Simplify        -> Trees    (simplifyAction trees)

        -- Check if the regex language is empty
            Empty           -> Strings  (emptyAction trees)

        -- Check if the regex language contains epsilon
            HasEpsilon      -> Strings  (hasEpsilonAction trees)

        -- Check if the regex language contains some non-empty string
            HasNonEpsilon   -> Strings  (hasNonEpsilonAction trees)

        -- Check if the regex language contains a string that contains one of the given characters
            Uses s          -> Strings  (usesAction s trees)

        -- Output a new regex whos language doesn't include a in any string
            NotUsing s      -> Trees  (notUsingAction s trees)

        -- Check if the regex language is infinite
            Infinite        -> Strings  (infiniteAction trees)

        -- Check if the regex language contains a string that starts with one of the given characters
            StartsWith s    -> Strings  (startsWithAction s trees)

        -- Reverse the regex, the trees will NOT be simplified
            Reverse         -> Trees    (reverseAction trees)

        -- Check if the regex language contains a string that ends with one of the given characters
            EndsWith s      -> Strings  (endsWithAction s trees)

        -- Output a new regex whos language denotes all prefixes of strings in the original language
            Prefixes        -> Trees    (prefixAction trees)




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
    -- note: "t@(Star _)" is a pattern that matches a Star node and binds it to t.
    simplifyTree (Star t@(Star _)) = simplifyTree t             -- t** = t*

    -- (s or e)* case
    simplifyTree (Star (Union t1 t2)) =
      let t1' = simplifyTree t1
          t2' = simplifyTree t2
      in case (t1', t2') of
           (Epsilon, _) -> simplifyTree (Star t2')              -- (e + s)* = s*
           (_, Epsilon) -> simplifyTree (Star t1')              -- (s + e)* = s*
           _ -> Star (Union t1' t2')                            -- Otherwise, keep the structure

    -- Star of empty set
    simplifyTree (Star Null) = Epsilon                          -- /* = e

    -- Star of epsilon
    simplifyTree (Star Epsilon) = Epsilon                       -- e* = e

    -- Simplify other stars
    simplifyTree (Star t) =
      let simplifiedT = simplifyTree t
      in case simplifiedT of
           Null -> Epsilon                                      -- /* = e
           Epsilon -> Epsilon                                   -- e* = e
           _ -> Star simplifiedT                                -- Otherwise, keep the star

    -- Union cases
    simplifyTree (Union t1 t2)
      | t1 == Null && t2 == Null = Null                         -- / + / = /
      | t1 == Null = simplifyTree t2                            -- / + t = t
      | t2 == Null = simplifyTree t1                            -- t + / = t
      | t1 == Epsilon && t2 == Epsilon = Epsilon                -- e + e = e
      | otherwise =
          let t1' = simplifyTree t1
              t2' = simplifyTree t2
          in case (t1', t2') of
                (Null, Null) -> Null                            -- / + / = /
                (Null, _)   -> t2'                              -- / + t = t
                (_, Null)   -> t1'                              -- t + / = t
                (Epsilon, _) | hasEpsilon t2' -> t2'            -- e + t = t if t has epsilon
                (_, Epsilon) | hasEpsilon t1' -> t1'            -- t + e = t if t has epsilon
                (Epsilon, Epsilon) -> Epsilon                   -- e + e = e
                _           -> Union t1' t2'                    -- (t + s)' = t' + s'


    -- Concat cases
    simplifyTree (Concat t1 t2)
      | t1 == Null || t2 == Null = Null                         -- / . t = /, t . / = /
      | t1 == Epsilon = simplifyTree t2                         -- e . t = t
      | t2 == Epsilon = simplifyTree t1                         -- t . e = t
      | otherwise =
          let t1' = simplifyTree t1
              t2' = simplifyTree t2
          in case (t1', t2') of
                (Null, _)    -> Null                            -- / . t = /
                (_, Null)    -> Null                            -- t . / = /
                (Epsilon, _) -> t2'                             -- e . t = t
                (_, Epsilon) -> t1'                             -- t . e = t
                _            -> Concat t1' t2'                  -- (s . t)' = s' . t'

    -- Base cases
    simplifyTree (Literal c)  = Literal c                       -- base case
    simplifyTree Epsilon      = Epsilon                         -- base case
    simplifyTree Null         = Null                            -- base case


-- Null Action
-- Checks if the language of each regex tree is empty
emptyAction :: [RegexTree] -> [String]
emptyAction trees = map (boolToString . isNull) (simplifyAction trees)
    where
        -- Helper function to check if a simplified tree is Null
        isNull :: RegexTree -> Bool
        isNull Null     = True
        isNull _        = False


-- HasEpsilon Action
-- Helper function to check if a simplified tree has Epsilon
hasEpsilon :: RegexTree -> Bool
hasEpsilon Epsilon  = True
hasEpsilon (Star _) = True
hasEpsilon (Union t1 t2) = hasEpsilon t1 || hasEpsilon t2
hasEpsilon (Concat t1 t2) = hasEpsilon t1 && hasEpsilon t2
hasEpsilon _ = False  -- For other cases, return False

-- Checks if the language of each regex tree contains epsilon
hasEpsilonAction :: [RegexTree] -> [String]
hasEpsilonAction trees = map (boolToString . hasEpsilon) (simplifyAction trees)


-- HasNonEpsilon Action
-- Helper function to check if a simplified tree has some non-empty string
hasNonEpsilon :: RegexTree -> Bool
hasNonEpsilon Epsilon = False
hasNonEpsilon Null = False
hasNonEpsilon _ = True  -- Possible thanks to the simplification

-- Checks if the language of each regex tree contains some non-empty string
hasNonEpsilonAction :: [RegexTree] -> [String]
hasNonEpsilonAction trees = map (boolToString . hasNonEpsilon) (simplifyAction trees)


-- Uses Action
-- Helper function to check if a tree uses any character from the string
uses :: String -> RegexTree -> Bool
uses chars (Literal c) = c `elem` chars
uses chars (Concat t1 t2) = uses chars t1 || uses chars t2
uses chars (Union t1 t2) = uses chars t1 || uses chars t2
uses chars (Star t) = uses chars t
uses _ Epsilon = False
uses _ Null = False

-- Check if the regex language contains a string that contains one of the given characters
usesAction :: String -> [RegexTree] -> [String]
usesAction s trees = map (boolToString . uses s) (simplifyAction trees)


-- NotUsing Action
-- Helper function to convert a tree to a new tree that doesn't use any character from the string
notUsing :: String -> RegexTree -> RegexTree
notUsing chars (Star t) = Star (notUsing chars t)
notUsing chars (Union t1 t2) = Union (notUsing chars t1) (notUsing chars t2)
notUsing chars (Concat t1 t2) = Concat (notUsing chars t1) (notUsing chars t2)
notUsing chars (Literal c) =
    if c `elem` chars then Null else Literal c
notUsing _ others = others

-- Output a new regex whos language doesn't include a in any string
notUsingAction :: String -> [RegexTree] -> [RegexTree]
notUsingAction s {-trees-} = map (notUsing s) {-trees-}


-- Infinite Action
-- Helper function to check if a simplified tree is infinite
isInfiniteRegex :: RegexTree -> Bool
isInfiniteRegex (Star _) = True  -- Star operator makes the language infinite
isInfiniteRegex (Union t1 t2) = isInfiniteRegex t1 || isInfiniteRegex t2
isInfiniteRegex (Concat t1 t2) = isInfiniteRegex t1 || isInfiniteRegex t2
isInfiniteRegex _ = False  -- For other cases, return False

-- Check if the regex language is infinite
infiniteAction :: [RegexTree] -> [String]
infiniteAction trees = map (boolToString . isInfiniteRegex) (simplifyAction trees)


-- StartsWith Action
-- Helper function to check if a tree's language starts with any character in s
startsWith :: String -> RegexTree -> Bool
startsWith chars (Concat t1 t2) =
    -- If t1 can be turned into Epsilon, also check t2
    if hasEpsilon t1 then startsWith chars t2 || startsWith chars t1
    else startsWith chars t1
startsWith chars (Union t1 t2) = startsWith chars t1 || startsWith chars t2
startsWith chars (Star t1) = startsWith chars t1
startsWith chars (Literal c) = c `elem` chars
startsWith _ _ = False

-- Check if the regex language contains a string that starts with one of the given characters
startsWithAction :: String -> [RegexTree] -> [String]
startsWithAction s trees = map (boolToString . startsWith s) (simplifyAction trees)

-- reverseAction
-- Helper function that reverses a tree
reverseTree :: RegexTree -> RegexTree
reverseTree (Star      t1)  = Star   (reverseTree t1)                   -- Go down the tree
reverseTree (Union  t1 t2)  = Union  (reverseTree t1) (reverseTree t2)  -- ditto
reverseTree (Concat t1 t2)  = Concat (reverseTree t2) (reverseTree t1)  -- Swap t1 and t2

reverseTree other           = other


-- Reverse the regex, the trees will NOT be simplified
reverseAction :: [RegexTree] -> [RegexTree]
reverseAction {-trees-} = map reverseTree {-trees-}

-- EndsWith Action
-- Helper function to check if a tree's language ends with any character in s
endsWith :: String -> RegexTree -> Bool
endsWith chars (Concat t1 t2) =
    -- If t2 can be turned into Epsilon, also check t1
    if hasEpsilon t2 then endsWith chars t1 || endsWith chars t2
    else endsWith chars t2
endsWith chars (Union t1 t2) = endsWith chars t1 || endsWith chars t2
endsWith chars (Star t1) = endsWith chars t1
endsWith chars (Literal c) = c `elem` chars
endsWith _ _ = False

-- Check if the regex language contains a string that ends with one of the given characters
endsWithAction :: String -> [RegexTree] -> [String]
endsWithAction s trees = map (boolToString . endsWith s) (simplifyAction trees)

-- TODO: This actually exposed a bug in simplifyAction where it doesn't fully simplify the regex.

-- Prefixes Action
prefixTree :: RegexTree -> RegexTree
prefixTree Epsilon = Epsilon
prefixTree Null = Null
prefixTree (Literal c) = Union (Literal c) Epsilon
prefixTree (Concat t1 t2) =
    Union (prefixTree t1)
          (Concat t1 (prefixTree t2))
prefixTree (Union t1 t2) =
    Union (prefixTree t1) (prefixTree t2)
prefixTree (Star t1) =
    Union Epsilon (Concat (prefixTree t1) (Star t1))

-- Output a new regex whos language denotes all prefixes of strings in the original language
prefixAction :: [RegexTree] -> [RegexTree]
prefixAction trees = simplifyAction (map prefixTree (simplifyAction trees))

-- The first simplification is required however for the second simplification... given how some tests
-- end up being over or under simplified, and this entire time I could not find a counter example
-- I am willing to conjecture that this function is correct regardless of whether you do the double
-- simplify or not. The regexes are not exact but they are equivalent, at least to my current knowledge.


