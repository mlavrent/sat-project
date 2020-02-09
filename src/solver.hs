{-# LANGUAGE LambdaCase #-}
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Foldable                  ( find )
import qualified Data.Map                      as Map
import           Data.Time
import           Text.Printf
import           Control.Exception
import           System.Environment
import           Control.DeepSeq
import           Control.Concurrent


-- Data definitions

type Variable = Int
data Literal = Pos Variable | Neg Variable deriving (Ord, Eq, Show)
type Clause = Set Literal
type SATInstance = Set Clause
data Result = Assignment [(Variable, Bool)] | Unsat


instance Show Result where
    show Unsat             = "UNSAT"
    show (Assignment assn) = foldl
        (\str (var, bool) -> str ++ show var ++ " " ++ showBool bool ++ " ")
        ""
        assn


instance NFData Result where
    rnf Unsat            = ()
    rnf (Assignment lst) = lst `deepseq` ()


-- Functions for defined data types

showBool :: Bool -> String
showBool b = if b then "true" else "false"

-- Functions to generate SAT Instance from DIMACS file

makeLiteral :: Int -> Literal
makeLiteral litInt =
    let absLit = abs litInt
    in
        (if litInt > 0
            then Pos absLit
            else if litInt < 0
                then Neg absLit
                else
                    error
                        "Error: Literal must be either postive or negative number"
        )

makeLiteralFromStr :: String -> Literal
makeLiteralFromStr litStr = makeLiteral (read litStr)

makeClause :: [String] -> Clause
makeClause = Set.fromList . map makeLiteralFromStr . init

litToVar :: Literal -> Variable
litToVar (Pos v) = v
litToVar (Neg v) = v

litToVarSigned :: Literal -> Variable
litToVarSigned (Pos v) = v
litToVarSigned (Neg v) = negate v

parseCNF :: String -> SATInstance
parseCNF input =
    let allLines     = lines input
        -- tokenize and remove comment lines
        tokLines     = map words (filter (/= "") allLines)
        contentLines = filter (\tokLine -> head tokLine /= "c") tokLines
        -- make sure first line is problem line
        pLine        = if head (head contentLines) /= "p"
            then error "Error: DIMACS file does not have problem line"
            else head contentLines
        -- Create a set of clauses from the content lines
        cnf = Set.fromList . map makeClause . tail $ contentLines
    in  assert (pLine !! 1 == "cnf")
            . assert (all (\clause -> last clause == "0") . tail $ contentLines)
            $ cnf


-- Functions to implement DPLL algorithm

getAllVars :: SATInstance -> Set Variable
getAllVars = Set.unions . Set.map unpackClause

unpackClause :: Clause -> Set Variable
unpackClause = Set.map litToVar

nextAssignment :: SATInstance -> Variable
nextAssignment cnf = 1

removeClausesWithLiteral :: Literal -> SATInstance -> SATInstance
removeClausesWithLiteral literal = Set.filter (literal `Set.notMember`)

removeNegatedLiteral :: Variable -> SATInstance -> SATInstance
removeNegatedLiteral v = Set.map
    (Set.filter
        (\case
            Pos cv -> cv /= negate v
            Neg cv -> cv /= negate v
        )
    )

unitClauseElim :: Result -> SATInstance -> (Result, SATInstance)
unitClauseElim Unsat cnf = (Unsat, cnf)
unitClauseElim (Assignment assn) cnf
    | Set.null cnf -- empty cnf
    = (Assignment assn, cnf)
    | otherwise -- get first unit clause, eliminate it, and recur
    = let unitClause = find (\c -> length c == 1) cnf
      in
          case unitClause of
              Nothing -> (Assignment assn, cnf) -- no unit clauses
              Just c  -> case literal of
                  Pos v -> unitClauseElim
                      (Assignment ((v, True) : assn))
                      (removeClausesWithLiteral literal
                                                (removeNegatedLiteral v cnf)
                      )
                  Neg v -> unitClauseElim
                      (Assignment ((v, False) : assn))
                      (removeClausesWithLiteral literal
                                                (removeNegatedLiteral v cnf)
                      )
                  where literal = Set.elemAt 0 c

assignmentAppend :: Result -> [(Variable, Bool)] -> Result
assignmentAppend Unsat             _        = Unsat
assignmentAppend (Assignment assn) new_assn = Assignment (assn ++ new_assn)


sameSignElim :: Result -> SATInstance -> (Result, SATInstance)
sameSignElim Unsat cnf = (Unsat, cnf)
sameSignElim assn cnf
    | Set.null cnf -- empty cnf
    = (assn, cnf)
    | otherwise -- get literal with same sign, eliminate it
    = let vars = concatMap (map litToVarSigned . Set.toList) $ Set.toList cnf
      in
          let
              sameSignLits = Set.fromList
                  (map
                      makeLiteral
                      (filter (\x -> (length . filter (== x)) vars == 1) vars)
                  )
          in
              let
                  new_assn =
                      ( Set.toList
                          . Set.map ((\v -> (v, v >= 0)) . litToVarSigned)
                          ) -- to fix: assignment contains negatives and pos
                          sameSignLits
              in  ( assignmentAppend assn new_assn
                  , Set.filter
                      (\s -> not $ Set.null $ Set.intersection s sameSignLits)
                      cnf
                  )




solveWithAssn :: Result -> SATInstance -> Result
solveWithAssn Unsat _ = Unsat
solveWithAssn (Assignment assn) cnf =
    fst $ (uncurry sameSignElim . unitClauseElim (Assignment assn)) cnf


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
    let result = solve . parseCNF $ contents  -- parse and solve
    end <- result `deepseq` getCurrentTime  -- force the computation and get end time
    print (formatOutput file (diffUTCTime end start) result) -- print it out
