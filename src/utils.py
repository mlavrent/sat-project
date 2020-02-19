
# Classes 

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
    def __init__(self, cid, literalSet):
        self.cid = cid
        self.literalSet = literalSet

    def __repr__(self):
        return f"{self.cid}: {str(self.literalSet)}"

    def __eq__(self, other):
        if type(other) != Clause:
            return False
        return self.cid == other.cid

# Heuristic functions

def rmoms(variable, clauseSet):
    # implementation of randomized MOMS (max occurances of min size)
    # TODO: Fix this and figure out what this variable is for
    smallClauseSize = 20
    k = 1.5

    negOcc = 0
    posOcc = 0

    posLit = Literal(variable, True)
    negLit = Literal(variable, False)

    for clause in clauseSet:
        if posLit in clause:
            posOcc += 1
        if negLit in clause:
            negOcc += 1

    return (posOcc + negOcc) * (2**k) + (posOcc * negOcc), posOcc > negOcc


def jerWang(variable, clauseSet):
    # implementation of Jeroslow-Wang
    posScore = 0
    negScore = 0
    for clause in clauseSet:
        if Literal(variable, True) in clause.literalSet:
            posScore += 2**(-len(clause.literalSet))
        if Literal(variable, False) in clause.literalSet:
            negScore += 2**(-len(clause.literalSet))

    return max(posScore, negScore), posScore > negScore


def rdlis(variable, clauseSet):
    # implementation of randomized DLIS (dynamic largest individual sum)
    negOcc = 0
    posOcc = 0

    posLit = Literal(variable, True)
    negLit = Literal(variable, False)

    for clause in clauseSet:
        if posLit in clause:
            posOcc += 1
        if negLit in clause:
            negOcc += 1

    return max(posOcc, negOcc), posOcc > negOcc
