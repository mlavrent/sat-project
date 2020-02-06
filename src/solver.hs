import qualified Data.Set                      as Set
import           Data.Foldable                  ( find )
import           Data.Time
import           Text.Printf
import           Control.Exception
import           System.Environment
import           Control.DeepSeq
import           Control.Concurrent


-- Data definitions

type Variable = Int
data Literal = Pos Variable | Neg Variable deriving (Ord, Eq, Show)
type Clause = Set.Set Literal
type SATInstance = Set.Set Clause
data Result = Assignment [(Variable, Bool)] | Unsat


instance Show Result where
    show Unsat             = "UNSAT"
    show (Assignment assn) = foldl
        (\str (var, bool) -> str ++ show var ++ " " ++ showBool bool)
        ""
        assn


instance NFData Result where
    rnf Unsat            = ()
    rnf (Assignment lst) = lst `deepseq` ()


-- Functions for defined data types

negateLiteral :: Literal -> Literal
negateLiteral literal = literal

showBool :: Bool -> String
showBool b = if b then "true" else "false"

-- Functions to generate SAT Instance from DIMACS file

makeLiteral :: String -> Literal
makeLiteral litStr =
    let
        litInt = read litStr
        absLit = abs litInt
    in
        (if litInt > 0
            then Pos absLit
            else if litInt < 0
                then Neg absLit
                else
                    error
                        "Error: Literal must be either postive or negative number"
        )


makeClause :: [String] -> Clause
makeClause = Set.fromList . map makeLiteral . init


parseCNF :: String -> SATInstance
parseCNF input =
    let
        allLines = lines input
        --tokenize and remove comment lines
        tokLines =
            map (\line -> words line) (filter (\line -> line /= "") allLines)
        contentLines = filter (\tokLine -> head tokLine /= "c") tokLines
        -- make sure first line is problem line
        pLine        = if head (head contentLines) /= "p"
            then error "Error: DIMACS file does not have problem line"
            else head contentLines
        -- Create a set of clauses from the content lines
        cnf = Set.fromList . map makeClause . tail $ contentLines
    in
        assert (pLine !! 1 == "cnf")
        . assert (all (\clause -> last clause == "0") . tail $ contentLines)
        $ cnf



-- Functions to implement DPLL algorithm

removeClausesWithLiteral :: Literal -> SATInstance -> SATInstance
removeClausesWithLiteral literal = Set.filter (literal `Set.notMember`)

removeNegatedLiteral :: Literal -> SATInstance -> SATInstance
removeNegatedLiteral literal =
    Set.map (Set.filter (\cliteral -> cliteral /= negateLiteral literal))

-- TODO: testing
unitClauseElim :: Result -> SATInstance -> (Result, SATInstance)
unitClauseElim Unsat satInst = (Unsat, satInst)
unitClauseElim assn satInst =
    ( assn
    , let maybeUnitClause = find (\clause -> Set.size clause == 1) satInst
      in
          case maybeUnitClause of
              Nothing -> satInst
              Just unitClause ->
                  let literal = Set.elemAt 0 unitClause
                  in  removeClausesWithLiteral
                          literal
                          (removeNegatedLiteral literal satInst)
    )


-- TODO: testing
sameSignElim :: Result -> SATInstance -> (Result, SATInstance)
sameSignElim Unsat satInst = (Unsat, satInst)
sameSignElim assn  satInst = (assn, satInst)


-- TODO 
solveWithAssn :: Result -> SATInstance -> Result
solveWithAssn Unsat             _   = Unsat
solveWithAssn (Assignment assn) cnf = Unsat -- TODO edit this here


-- TODO
solve :: SATInstance -> Result
solve = solveWithAssn (Assignment [])


formatOutput :: String -> NominalDiffTime -> Result -> String
formatOutput file runTime res =
    printf "Instance %s Time: %s Result: %s" file (show runTime) (show res)


main :: IO ()
main = do
    args <- getArgs
    let file = head args
    contents <- readFile file
    start    <- getCurrentTime
    let result = solve . parseCNF $ contents
    end <- result `deepseq` getCurrentTime
    print (formatOutput file (diffUTCTime end start) result)
