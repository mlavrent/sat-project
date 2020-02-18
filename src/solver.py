import sys
from time import time
from copy import copy, deepcopy
import random
from multiprocessing import Process, Queue
from queue import Empty


class Literal:
    def __init__(self, name, sign):
        self.name = name  # integer
        self.sign = sign  # boolean

    def __repr__(self):
        return ("-" if not self.sign else "") + self.name

    def __eq__(self, other):
        if type(other) != Literal:
            return False
        return self.name == other.name and self.sign == other.sign

    def __hash__(self):
      return hash((self.name, self.sign))


class Clause:
    def __init__(self, id, literalSet):
        self.id = id
        self.literalSet = literalSet

        # indices of the watched literals in literalSet
        self.watchIdx = [0, 1]
        self.numLits = len(literalSet)
    
    def removeLit(self, lit, vars, clauseSet, assignment):
        watchLit0 = self.literalSet[self.watchIdx[0]]
        watchLit1 = self.literalSet[self.watchIdx[1]]

        self.literalSet.remove(lit)
        self.numLits -= 1

        if lit == watchLit0:
            # choose another watched literal
            if self.numLits >= 2:
                self.watchIdx[0] = 1
            else:
                # if we can't, unit propogate the other literal immediately
                propogateUnit(watchLit1, vars, clauseSet, assignment)
                
        elif lit == watchLit1:
            # choose another watched literal
            if self.numLits >= 2:
                self.watchIdx[1] = 1
            else:
                # if we can't, unit propogate the other literal immediately
                propogateUnit(watchLit0, vars, clauseSet, assignment)

    def removeFromSet(self, clauseSet):
        try:
            clauseSet.remove(self)
        except ValueError:
            return

    def __repr__(self):
        return "{}: {}".format(self.id, self.literalSet)

    def __eq__(self, other):
        if type(other) != Clause:
            return False
        return self.id == other.id


def rmoms(var, clauseSet):
    # implementation of randomized MOMS (max occurances of min size)
    smallClauseSize = 20
    k = 1.5

    negOcc = 0
    posOcc = 0

    posLit = Literal(var, True)
    negLit = Literal(var, False)

    for clause in clauseSet:
        if posLit in clause.literalSet:
            posOcc += 1
        if negLit in clause.literalSet:
            negOcc += 1

    return (posOcc + negOcc) * (2 ** k) + (posOcc * negOcc), posOcc > negOcc


def jerWang(var, clauseSet):
    # implementation of Jeroslow-Wang
    posScore = 0
    negScore = 0
    for clause in clauseSet:
        if Literal(var, True) in clause.literalSet:
            posScore += 2 ** (-clause.numLits)
        if Literal(var, False) in clause.literalSet:
            negScore += 2 ** (-clause.numLits)

    return max(posScore, negScore), posScore > negScore


def rdlis(var, clauseSet):
    # implementation of randomized DLIS (dynamic largest individual sum)
    negOcc = 0
    posOcc = 0

    posLit = Literal(var, True)
    negLit = Literal(var, False)

    for clause in clauseSet:
        if posLit in clause.literalSet:
            posOcc += 1
        if negLit in clause.literalSet:
            negOcc += 1

    return max(posOcc, negOcc), posOcc > negOcc


def chooseVariableSplit(vars, clauseSet, heuristic):
    # dictionary of our top candidates of form {var: (score, sign)}
    topCands = {}
    for v in vars:
        vscore, vsign = heuristic(v, clauseSet)

        # if top candidates not full, add to it
        if len(topCands) < 5:
            topCands[v] = (vscore, vsign)
            continue

        # otherwise replace worst candidate if better
        worstTopCand = None
        for cand in topCands:
            if worstTopCand is None or topCands[cand][0] < topCands[worstTopCand][0]:
                worstTopCand = cand
        
        if vscore > topCands[worstTopCand][0]:
            del topCands[worstTopCand]
            topCands[v] = (vscore, vsign)

    # randomly choose a top candidate              
    var, scoreSign = random.choice(list(topCands.items()))
    return var, scoreSign[1]


def propogateUnit(unitLit, vars, clauseSet, assignment):
    unitLitInv = Literal(unitLit.name, not unitLit.sign)

    assignment[unitLit.name] = unitLit.sign
    # this var might've already been removed - skip it
    try:
        vars.remove(unitLit.name)
    except ValueError:
        return

    for otherClause in copy(clauseSet):
        # remove everywhere inverse shows up
        for otherLit in copy(otherClause.literalSet):
            if otherLit == unitLitInv:
                otherClause.removeLit(otherLit, vars, clauseSet, assignment)

        # remove clauses containing the normal literal
        if otherClause.numLits > 1 and unitLit in otherClause.literalSet:
            otherClause.removeFromSet(clauseSet)
    

def unitClauseElim(vars, clauseSet, assignment):
    changed = False

    for clause in copy(clauseSet):
        if clause.numLits == 1:
            changed = True
            propogateUnit(clause.literalSet[0], vars, clauseSet, assignment) 
    
    return assignment, changed


def sameSignElim(vars, clauseSet, assignment):
    changed = False

    # get all literals
    allLiterals = set()
    for clause in clauseSet:
        for lit in clause.literalSet:
            allLiterals.add(lit)

    # find pure literals, remove them, and add to assignment
    for lit in copy(allLiterals):
        oppLit = Literal(lit.name, not lit.sign)
        if oppLit not in allLiterals:
            assignment[lit.name] = lit.sign
            if lit.name in vars:
                vars.remove(lit.name)
            changed = True

            # remove all clauses containing that literal
            for clause in copy(clauseSet):
                if lit in clause.literalSet:
                    clause.removeFromSet(clauseSet)

    return assignment, changed


def assignVariable(var, sign, vars, clauseSet, assignment):
    assignment[var] = sign
    vars.remove(var)

    for clause in copy(clauseSet):
        for lit in copy(clause.literalSet):
            if lit.name == var and lit.sign == sign:
                # remove this clause and stop checking this clause's literals
                clause.removeFromSet(clauseSet)
                break
            elif lit.name == var and lit.sign != sign:
                clause.removeLit(lit, vars, clauseSet, assignment)
    
    return clauseSet, assignment


def solve(vars, clauseSet, assignment):
    # do inference with unit clause and same sign elim until we can't anymore
    changed = True
    while changed:
        assignment, uceChange = unitClauseElim(vars, clauseSet, assignment)
        assignment, sseChange = sameSignElim(vars, clauseSet, assignment)
        changed = uceChange or sseChange


    if any(c.literalSet == [] for c in clauseSet):
        # if there's an empty clause, it's UNSAT
        return None
    elif not clauseSet:
        # if there are no clauses, it's SAT
        return assignment

    # decide which variable to split on
    var, sign = chooseVariableSplit(vars, clauseSet, rmoms)

    # try solving down the first branch
    fVars = deepcopy(vars)
    fClauseSet = deepcopy(clauseSet)
    fAssignment = deepcopy(assignment)

    fClauseSet, fAssignment = assignVariable(var, sign, fVars, fClauseSet, fAssignment)
    fAssignment = solve(fVars, fClauseSet, fAssignment)

    # if this branch isn't UNSAT, return its assignment - it's a solution
    if fAssignment is not None:
        return fAssignment

    # try solving down the second branch
    clauseSet, assignment = assignVariable(var, not sign, vars, clauseSet, assignment)
    return solve(vars, clauseSet, assignment)


def runSolver(conn, vars, clauseSet):
    assignment = solve(copy(vars), clauseSet, {})
    # assign any variable not already assigned to true (if SAT)
    if assignment is not None:
        for v in vars:
            if v not in assignment:
                assignment[v] = True

    conn.put(assignment)


def verifySolution(assignment, clauseSet):
    for clause in clauseSet:
        clauseSatisfied = False
        for lit in clause.literalSet:
            # this literal satisfied the clause
            if assignment[lit.name] == lit.sign:
                clauseSatisfied = True
                break

        # this clause not satisfied
        if not clauseSatisfied:
            return False

    # all clauses satisfied
    return True


def readInput(cnfFile):
    variableSet = []
    clauseSet = []
    nextCID = 0
    with open(cnfFile, "r") as f:
        for line in f.readlines():
            tokens = line.strip().split()
            if tokens and tokens[0] != "p" and tokens[0] != "c":
                literalSet = []
                for lit in tokens[:-1]:
                    sign = lit[0] != "-"
                    variable = lit.strip("-")

                    literalSet.append(Literal(variable, sign))
                    if variable not in variableSet:
                        variableSet.append(variable)

                clauseSet.append(Clause(nextCID, literalSet))
                nextCID += 1
    
    return variableSet, clauseSet


def printOutput(file, assignment, runTime):
    result = ""
    if assignment is not None:
        result = "SAT Solution:"
        for var in assignment:
            result += " " + str(var) + " " + ("true" if assignment[var] else "false")
    else:
        result = "UNSAT"

    print("Instance: {} Time: {:.2f} Result: {}".format(file, runTime, result))


if __name__ == "__main__":
    inputFile = sys.argv[1]
    varSet, clauseSet = readInput(inputFile)
    verifClauseSet = deepcopy(clauseSet)

    queueConn = Queue()
    solverProcess = Process(target=runSolver, args=(queueConn, varSet, clauseSet))

    startTime = time()

    # do a restart, with growing time increments
    timeout = 30
    timeoutMult = 2
    solverProcess.start()
    while True:
        try:
            assignment = queueConn.get(block=True, timeout=timeout)
            break
        except Empty:
            # kill solver and restart
            timeout *= timeoutMult
            print("Restarting solver with timeout {:.2f}s".format(timeout))

            solverProcess.terminate()
            solverProcess = Process(target=runSolver, args=(queueConn, varSet, clauseSet))
            solverProcess.start()

    runtime = time() - startTime

    if assignment is not None:
        print("Verifying solution: {}".format(verifySolution(assignment, verfClauseSet)))
    printOutput(inputFile, assignment, runtime)
