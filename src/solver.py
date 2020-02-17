#!/usr/bin/python3
import sys
import random
from time import time
from copy import copy, deepcopy


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
    def __init__(self, literalSet):
       self.literalSet = literalSet  # set(literal)

    def __repr__(self):
        return str(self.literalSet)

    def __eq__(self, other):
        return self.literalSet == other.literalSet


def rmoms(vars, clauseSet):
    # implementation of randomized MOMS (max occurances of min size)
    ...
    return "x", True


def jerWang(vars, clauseSet):
    # implementation of Jeroslow-Wang
    ...
    return "x", True


def rdlis(vars, clauseSet):
    # implementation of randomized DLIS (dynamic largest individual sum)
    ...
    return "x", True


def chooseVariableSplit(vars, clauseSet):
    ...
    return "x", True


def unitClauseElim(vars, clauseSet, assignment):
    changed = True
    while changed:
        changed = False

        for clause in clauseSet:
            if len(clause.literalSet) == 1:
                unitLit = clause.literalSet[0]
                unitLitInv = Literal(unitLit.name, not unitLit.sign)

                assignment[unitLit.name] = unitLit.sign
                vars.remove(unitLit)
                changed = True

                for otherClause in clauseSet:
                    # remove everywhere inverse shows up
                    for otherLit in otherClause.literalSet:
                        if otherLit == unitLitInv:
                            otherClause.literalSet.remove(otherLit)

                    # remove clauses containing the normal literal
                    if len(otherClause.literalSet) > 1 and unitLit in otherClause.literalSet:
                        clauseSet.remove(otherClause)
    
    return assignment


def sameSignElim(vars, clauseSet, assignment):
    changed = True
    while changed:
        changed = False

        # get all literals
        allLiterals = set()
        for clause in clauseSet:
            for lit in clause.literalSet:
                allLiterals.add(lit)

        # find pure literals, remove them, and add to assignment
        for lit in allLiterals:
            oppLit = Literal(lit.name, not lit.sign)
            if oppLit in allLiterals:
                allLiterals.remove(oppLit)
            else:
                assignment[lit.name] = lit.sign
                changed = True

                # remove all clauses containing that literal
                for clause in clauseSet:
                    clauseSet.remove(clause)
    
    return assignment


def assignVariable(var, sign, vars, clauseSet, assignment):
    assignment[var] = sign
    vars.remove(var)

    for clause in clauseSet:
        for lit in clause.literalSet:
            if lit.name == var and lit.sign == sign:
                clauseSet.remove(clause)
            elif lit.name == var and lit.sign != sign:
                clause.literalSet.remove(lit)
    
    return clauseSet, assignment


def solve(vars, clauseSet, assignment):
    # do inference with unit clause and same sign elim
    assignment = unitClauseElim(vars, clauseSet, assignment)
    assignment = sameSignElim(vars, clauseSet, assignment)

    if Clause([]) in clauseSet:
        # if there's an empty clause, it's UNSAT
        return None
    elif not clauseSet:
        # if there are no clauses, it's SAT
        return assignment

    # decide which variable to split on
    var, sign = chooseVariableSplit(vars, clauseSet)

    # try solving down the first branch
    fVars = deepcopy(vars)
    fClauseSet = deepcopy(vars)
    fAssignment = deepcopy(assignment)

    fClauseSet, fAssignment = assignVariable(var, sign, fVars, fClauseSet, fAssignment)
    fAssignment = solve(fVars, fClauseSet, fAssignment)

    # if this branch isn't UNSAT, return its assignment - it's a solution
    if fAssignment is not None:
        return fAssignment

    # try solving down the second branch
    clauseSet, assignment = assignVariable(var, not sign, vars, clauseSet, assignment)
    return solve(vars, clauseSet, assignment)


def readInput(cnfFile):
    variableSet = []
    clauseSet = []
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

                clauseSet.append(Clause(literalSet))
    
    return variableSet, clauseSet


def printOutput(file, assignment, runTime):
    result = ""
    if assignment is not None:
        result = "SAT Solution:"
        for var in assignment:
            result += " " + str(var) + ("true" if assignment[var] else "false")
    else:
        result = "UNSAT"

    print(f"Instance: {file} Time: {runTime:.2f} Result: {result}")


if __name__ == "__main__":
    inputFile = sys.argv[1]
    varSet, clauseSet = readInput(inputFile)

    # assignment will be None if unsat
    startTime = time()
    assignment = solve(varSet, clauseSet, {})
    runtime = time() - startTime

    printOutput(inputFile, assignment, runtime)