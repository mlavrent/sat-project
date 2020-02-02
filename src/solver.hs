import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time
import Text.Printf
import Control.Exception
import System.Environment


type Variable = Int
data Literal = Pos Variable | Neg Variable deriving (Ord, Eq, Show)
type Clause = Set Literal
type SATInstance = Set Clause
data Result = Assignment [(Variable, Bool)] | Unsat


showBool :: Bool -> String
showBool b = if b then "true" else "false"

instance Show Result where
    show Unsat = "UNSAT"
    show (Assignment assn) = foldl (\ str (var, bool) -> str ++ (show var) ++ " " ++ (showBool bool)) "" assn


unitClauseElim :: Result -> SATInstance -> (SATInstance, Result)
unitClauseElim assn satInst = (satInst, assn)


sameSignElim :: Result -> SATInstance -> (SATInstance, Result)
sameSignElim assn satInst = (satInst, assn)


makeLiteral :: String -> Literal
makeLiteral litStr = 
    let litInt = read litStr
        absLit = abs litInt
    in (if litInt > 0
            then Pos absLit
        else if litInt < 0
            then Neg absLit
        else error "Error: Literal must be either postive or negative number")


makeClause :: [String] -> Clause
makeClause = Set.fromList . (map makeLiteral) . init


parseCNF :: String -> SATInstance
parseCNF input = 
    let allLines = lines input
        --tokenize and remove comment lines
        tokLines = map (\ line -> words line) (filter (\ line -> line /= "") allLines)
        contentLines = filter (\ tokLine -> head tokLine /= "c") tokLines
        -- make sure first line is problem line
        pLine = if head (head contentLines) /= "p" 
                    then error "Error: DIMACS file does not have problem line" 
                else head contentLines
        -- Create a set of clauses from the content lines
        cnf = Set.fromList . (map makeClause) . tail $ contentLines
    in (assert (pLine!!1 == "cnf")) . (assert (all (\ clause -> last clause == "0") . tail $ contentLines)) $ cnf


solve :: SATInstance -> Result
solve satInst = Unsat


formatOutput :: String -> Result -> String
formatOutput fileName res =
    printf "Instance: %s Time: -- Result: %s" fileName (show res)


main :: IO ()
main = do
    args <- getArgs
    let file = head args
    print file
    contents <- readFile file
    let result = solve . parseCNF $ contents
    print (formatOutput file result)