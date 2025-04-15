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
        <> help "Check if the regex language does not contain a string that contains one of the given characters" )

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

        -- Check if the regex language does not contain a string that contains one of the given characters
            NotUsing s      -> Strings  (notUsingAction s trees)




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
      | t1 == Epsilon = Union Epsilon (simplifyTree t2)         -- e + t = t
      | t2 == Epsilon = Union (simplifyTree t1) Epsilon         -- t + e = t
      | otherwise =
          let t1' = simplifyTree t1
              t2' = simplifyTree t2
          in case (t1', t2') of
                (Null, Null) -> Null                            -- / + / = /
                (Null, _)   -> t2'                              -- / + t = t
                (_, Null)   -> t1'                              -- t + / = t
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
emptyAction trees = map isNull (simplifyAction trees)
    where
        -- Helper function to check if a simplified tree is Null
        isNull :: RegexTree -> String
        isNull Null     = "yes"
        isNull _        = "no"


-- HasEpsilon Action
-- Checks if the language of each regex tree contains epsilon
hasEpsilonAction :: [RegexTree] -> [String]
hasEpsilonAction trees = map hasEpsilon (simplifyAction trees)
    where
        -- Helper function to check if a simplified tree has Epsilon
        hasEpsilon :: RegexTree -> String
        hasEpsilon Epsilon  = "yes"
        hasEpsilon (Star _) = "yes"
        hasEpsilon (Union t1 t2)    = if hasEpsilon t1 == "yes" || hasEpsilon t2 == "yes"
                                        then "yes"
                                        else "no"
        hasEpsilon (Concat t1 t2)   = if hasEpsilon t1 == "yes" && hasEpsilon t2 == "yes"
                                        then "yes"
                                        else "no"
        hasEpsilon _ = "no"  -- For other cases, return "no"

-- HasNonEpsilon Action
-- Checks if the language of each regex tree contains some non-empty string
hasNonEpsilonAction :: [RegexTree] -> [String]
hasNonEpsilonAction trees = map hasNonEpsilon (simplifyAction trees)
    where
        -- Helper function to check if a simplified tree has some non-empty string
        hasNonEpsilon :: RegexTree -> String
        hasNonEpsilon Epsilon   = "no"
        hasNonEpsilon Null      = "no"
        hasNonEpsilon _         = "yes"  -- Possible thanks to the simplifcation

-- Uses Action
-- Check if the regex language contains a string that contains one of the given characters
usesAction :: String -> [RegexTree] -> [String]
usesAction s trees = map (uses s) (simplifyAction trees)
    where
        -- Helper function to check if a tree uses any character from the string
        uses :: String -> RegexTree -> String
        uses chars (Literal c)      = if c `elem` chars then "yes" else "no"
        uses chars (Concat t1 t2)   = if uses chars t1 == "yes" || uses chars t2 == "yes"
                                        then "yes"
                                        else "no"
        uses chars (Union t1 t2)    = if uses chars t1 == "yes" || uses chars t2 == "yes"
                                        then "yes"
                                        else "no"
        uses chars (Star t)         = uses chars t
        uses _ Epsilon              = "no"
        uses _ Null                 = "no"

-- NotUsing Action
-- Check if the regex language does not contain a string that contains one of the given characters
notUsingAction :: String -> [RegexTree] -> [String]
notUsingAction s trees = map (notUsing s) (simplifyAction trees)
    where
        -- Helper function to check if a tree does not use any character from the string
        notUsing :: String -> RegexTree -> String
        notUsing chars (Literal c)      = if c `elem` chars then "no" else "yes"
        notUsing chars (Concat t1 t2)   = if notUsing chars t1 == "no" && notUsing chars t2 == "no"
                                            then "no"
                                            else "yes"
        notUsing chars (Union t1 t2)    = if notUsing chars t1 == "no" && notUsing chars t2 == "no"
                                            then "no"
                                            else "yes"
        notUsing chars (Star t)         = notUsing chars t
        notUsing _ Epsilon              = "yes"
        notUsing _ Null                 = "yes"