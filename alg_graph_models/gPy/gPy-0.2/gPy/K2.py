from gPy.Graphs import ADG
from gPy.FKMCMC import OrderGraphSearch

_version = '$Id: K2.py,v 1.1 2008/10/07 09:11:01 jc Exp $'

class K2(OrderGraphSearch):
    def __init__(self, max_parents_family):
        # maximum number of parents in a family
        self._max_parents_family = max_parents_family

    def search(self, data, order, score=None):
        arr = []
        parents = {}
        if score is None:
            def score(data,child,parents):
                return data.family_score(child, parents)
        for i, child in enumerate(order):
            # candidate parents are those variables occuring before child in
            # the order
            candidate_parents = set(order[:i])
            parents[child] = self._find_parents(data, child, candidate_parents,score)
            for parent in parents[child]:
                arr.append((parent,child))

        return ADG(vertices=order, arrows=arr)

    def _find_parents(self, data, child, candidate_parents, score):
        # the parents of this child is initially an empty set
        parents = set()
        best_score = score(data,child, parents)
        while True:
            # a value of None indicates no better parent was found.
            best_new_parent = None
            for new_parent in candidate_parents:
                new_score = score(data,child, parents | set([new_parent]))
                if new_score > best_score:
                    best_new_parent = new_parent
                    best_score = new_score

            if best_new_parent is None:
                break

            parents.add(best_new_parent)
            candidate_parents.remove(best_new_parent)
            if len(parents) > self._max_parents_family:
                break

        return parents

# try:
#     import psyco

#     psyco.bind(K2)
# except:
#     pass

