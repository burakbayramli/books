"""
Various utilities for interfacing with weighted SAT solvers, particularly
(altered versions of) MaxWalkSAT
"""

_version = '$Id: WSATUtils.py,v 1.1 2008/10/07 09:15:01 jc Exp $'

def best_n(wcnf_filename,output_filename,best,set_true=(),flags=''):
    """Get C{n} best ADGs

    @param wcnf_filename: Name of WCNF file
    @type wcnf_filename: String
    @param output_filename: Where C{true_adg} will be if it is found in the search.
    @type output_filename: String
    @param best: How many ADGs to return
    @type best: Integer
    @param set_true: Sequence of atoms to initialise to true (only for cheating!)
    @type set_true: Sequence
    @param flags: Options to give newmaxwalksat
    @type flags: String
    """
    write_initfile(set_true)
    write_printatomsinitfile()
    cmd = 'newmaxwalksat -init initfile -printatoms printatomsinitfile ' + flags + ' < ' +  wcnf_filename + " | grep '^C [0-9]*' | sort -u | sort -n -k2,2 > /tmp/zzz; head -n " + str(best) + " /tmp/zzz > " + output_filename
    print cmd
    import os
    os.system(cmd)


def find_it(wcnf_filename,output_filename,true_adg,inv_atom_ids,predicate='has_parents',set_true=(),flags=''):
    """Method for searching for a true ADG

    @param wcnf_filename: Name of WCNF file
    @type wcnf_filename: String
    @param output_filename: Where C{true_adg} will be if it is found in the search.
    @type output_filename: String
    @param true_adg: The true ADG
    @type true_adg: L{Graphs.ADG}
    @param inv_atom_ids: Mapping from logical atoms to numbers
    @type inv_atom_ids: Dictionary
    @param predicate: The predicate symbol used for the relation beween a child
    and its parent set
    @type predicate: String
    @param set_true: Sequence of atoms to initialise to true (only for cheating!)
    @type set_true: Sequence
    @param flags: Options to give newmaxwalksat
    @type flags: String
    @return: Whether C{true_adg} is amongst those found in the search
    @rtype: Boolean
    """
    import os
    write_initfile(set_true)
    encoding_string = ' '.join(sorted(true_adg.encoding_family(inv_atom_ids,predicate)))
    p_init = open('printatomsinitfile','w')
    for atom in encoding_string.split():
        print >>p_init, atom
    p_init.close()
    cmd = 'newmaxwalksat -init initfile -printatoms printatomsinitfile -printall ' + flags + ' < ' +  wcnf_filename + " | grep '^C [0-9]*  " + encoding_string + "' > " + output_filename
    print cmd
    os.system(cmd)

    # is there a line there?
    return bool(open(output_filename).read())


def make_sample(wcnf_filename,output_filename,best,set_true=(),flags=''):
    """Generate a sample

    @param wcnf_filename: Name of WCNF file
    @type wcnf_filename: String
    @param output_filename: Where C{true_adg} will be if it is found in the search.
    @type output_filename: String
    @param best: How many ADGs to return
    @type best: Integer
    @param set_true: Sequence of atoms to initialise to true (only for cheating!)
    @type set_true: Sequence
    @param flags: Options to give newmaxwalksat
    @type flags: String
    """
    write_initfile(set_true)
    write_printatomsinitfile()
    cmd = 'newmaxwalksat -init initfile -printatoms printatomsinitfile ' + flags + ' < ' +  wcnf_filename + " | grep '^C [0-9]*' | sort -u < " + output_filename
    print cmd
    import os
    os.system(cmd)


def parse_maxwalksat(fobj,inv_atom_ids,predicate='has_parents'):
    """
    Return (cost_of_ADG,ADG) pairs from MaxWalkSAT output

    The method looks for output between 'Begin assign' and 'End assign'

    @param fobj: MaxWalkSAT output
    @type fobj: Readable file object (B{not} the filename)
    @param inv_atom_ids: Maps integers to ground logical atoms. This will
    have typically been produced by L{write_cnf}.
    @type inv_atom_ids: List
    @param predicate: The ground logical atoms recovered using C{inv_atom_ids} are assumed to be
    tuples of the form C(predicate,child,pas)} where C{child} is a vertex and C{pas} is a frozenset
    giving its parents.
    @type predicate: String
    @return: Returns (cost_of_ADG,ADG) pairs for all ADGs in the output.
    @rtype: List
    @raise IndexError: If an integer is encountered for which C{inv_atom_ids} can find no ground logical
    atom.
    """
    import re
    from Graphs import ADG
    pattern = re.compile(
        r'Begin assign.*?(\d+).*?\n(.*?)End assign',re.DOTALL)
    costed_adgs = []
    for match in re.finditer(pattern,fobj.read()):
        cost = int(match.group(1))
        adg = ADG()
        for atom_int in match.group(2).split():
            atom = inv_atom_ids[int(atom_int)]
            if atom[0] == predicate:
                adg.put_family(atom[1],atom[2])
        costed_adgs.append((cost,adg))
    return costed_adgs





def runwalksat(wcnf_filename,output_filename,set_true=(),flags='',printatoms=False):
    """Basic routine to run maxwalksat

    Actually calls 'newmaxwalksat', an altered version of  MaxWalkSAT
    
    @param wcnf_filename: Name of WCNF file
    @type wcnf_filename: String
    @param output_filename: Name of output file
    @type output_filename: String
    @param set_true: Atoms to set initially true (useful for cheating)
    @type set_true: Iterable    
    @param flags: Additional flags to give to MaxWalkSAT
    @type flags: String
    @param printatoms: Which atoms to select for printing during a run
    @type printatoms: Iterable
    """
    for line in open(wcnf_filename):
        if line.startswith('p wcnf'):
            num_atoms = int(line.split()[3])
            break
    write_initfile(num_atoms,set_true)
    if printatoms:
        write_printatomsinitfile()
        cmd = 'newmaxwalksat -init initfile -printatoms printatomsinitfile ' + flags + ' < ' + wcnf_filename + " | grep '^C [0-9]*' | sort -u > " + output_filename
    elif '-printall' in flags:
        cmd = 'newmaxwalksat -init initfile ' + flags + ' < ' + wcnf_filename + " | grep '^C [0-9]*' | sort -u > " + output_filename
    else:
        cmd = 'newmaxwalksat ' + flags + ' < ' + wcnf_filename + " > " + output_filename
    print cmd
    import os
    os.system(cmd)


def write_cnf(file,clause_generators,hard_clause_cost=None,comments=True):
    """
    Write a CNF file using the provided C{clause_generators}
        
    Ideally the cost of a hard clause is +infinity, but ...
    If the cost of hard clauses is too high then an assignment with an overflowing
    total cost may be produced, possibly confusing a SAT solver.
    If it is too low, then an assignment which breaks a hard clause may be optimal.

    The default is to set it to x such that cost_of_soft_clauses * x|hard clauses| = sys.maxint.
    Then even breaking all clauses will not give an overflow.
    This is reasonable as long as the SAT solver will be running on the same machine as this Python
    process.  If this value of x is below the cost of a soft clause an error is raised. 

    This method returns the tuple (atom_ids, inv_atom_ids, extra_cost).
    atom_ids is a dictionary mapping logical ground atoms to the integers representing them
    inv_atom_ids is the inverse of atom_ids. It is a list (note that inv_atom_ids[0]=None, since
    0 is never used to represent an atom in WCNF files.)
    extra_cost is the total cost of all soft empty clauses which (ideally) would be in the WCNF file. Such
    clauses are B{not} written to the WCNF file since solvers (eg UBCSAT) don't like them. This cost should be
    added to the cost of any assigment found by a SAT solver.

    @param file: Where to write the WCNF file
    @type file: Writable file object
    @param clause_generators: Sequence of clause generator iterators. For example, C{clause_generators = (
    probadg.clauses_justorder(float_weights=True), probadg.clauses_no3cycles())} is a valid choice, where C{probadg}
    is a L{RandomGraphs.RandomADG} object.
    @type clause_generators: Sequence
    @param hard_clause_cost: The cost of breaking a 'hard' clause. If C{None} a suitable value is computed internally.
    @type hard_clause_cost: Number of C{None}
    @param comments: Whether to include comments in the generated WCNF stating what the atoms and clauses mean
    @type comments: Boolean
    @return: atom_ids, inv_atom_ids, extra_cost (see above)
    @rtype: Tuple
    @raise RuntimeError: If a hard empty clause is generated. (Such a clause can never be satisfied.)
    @raise ValueError: If soft clauses have such high costs that a sensible hard clause cost cannot be chosen.
    """
    import sys

    atom_ids = {}
    inv_atom_ids = [None]
    current_atom = 1
    encoded_clauses = []
    cost_of_soft_clauses = 0
    num_hard_clauses = 0
    maxsoftcost = 0
    extra_cost = 0

    for clause_generator in clause_generators:
        for clause in clause_generator:
            cost = clause[0]

            if clause[1:] == ():
                if cost == 'hard':
                    raise RuntimeError("Can't have empty hard clauses")
                else:
                    print 'Removing soft empty clause %s from WCNF file' % clause
                    extra_cost += cost
                    continue

            if cost == 'hard':
                num_hard_clauses += 1
            else:
                cost_of_soft_clauses += cost
                maxsoftcost = max(maxsoftcost,cost)
            encoded_clause = ''
            for lit in clause[1:]:
                atom = lit[1:]
                try:
                    atom_id = atom_ids[atom]
                except KeyError:
                    atom_ids[atom] = current_atom
                    inv_atom_ids.append(atom)
                    atom_id = current_atom
                    if comments:
                        print >>file, 'c Atom %d means %s' % (atom_id,atom)  
                    current_atom += 1
                if lit[0] is True:
                    encoded_lit = atom_id
                else:
                    encoded_lit = -atom_id
                encoded_clause += '%s ' % encoded_lit
            encoded_clauses.append((cost,encoded_clause))
    if hard_clause_cost is None and num_hard_clauses > 0:
        hard_clause_cost = int((sys.maxint - cost_of_soft_clauses) / num_hard_clauses)
        if hard_clause_cost <= maxsoftcost:
            raise ValueError("Want to set cost of hard clauses to %d but there's a soft clause with weight %d" % (
                hard_clause_cost,maxsoftcost))
    if comments:
        for encoded_weighted_clause in encoded_clauses:
            cost, encoded_clause = encoded_weighted_clause
            clause = ''
            for num in encoded_clause.rstrip().split(' '):
                    num = int(num)
                    if num > 0:
                        clause += '%s ' % (inv_atom_ids[num],)
                    else:
                        clause += '-%s '% (inv_atom_ids[-num],)
            if cost == 'hard':
                cost = hard_clause_cost
            print >>file, 'c %s %s0 means %s %s' % (cost, encoded_clause, cost, clause)

    print >>file, 'p wcnf %d %d' % (current_atom-1,len(encoded_clauses))
    for encoded_weighted_clause in encoded_clauses:
        cost, encoded_clause = encoded_weighted_clause
        if cost == 'hard':
            cost = hard_clause_cost
        print >>file, '%s %s0' % (cost, encoded_clause)

    # if we need to recover stuff from SAT solver output
    # first maps atoms to numbers
    # second maps numbers to atoms
    file.close()
    return atom_ids, inv_atom_ids, extra_cost


def write_initfile(num_atoms,set_true=frozenset()):
    """
    Writes a file called 'initfile' in the current directory.
    This file will be used to set all atoms, except those in C{set_true} initially to false
    in a MaxWalkSAT run

    @param num_atoms: The number of atoms to set. The set of atoms is
    1,2, ... num_atoms
    @type num_atoms: Integer
    @param set_true: Atoms to set true (useful for cheating)
    @type set_true: Iterable
    
    """
    init = open('initfile','w')
    for atom in range(1,num_atoms+1):
        if atom in set_true:
            print >>init, atom
        else:
            print >>init, -atom
    init.close()

def write_printatomsinitfile(printatoms):
    """
    Writes a file called 'printatomsinitfile' in the current directory.
    This file will be used to tell MaxWalkSAT which atoms to print out during its run

    @param printatoms: Which atoms to print out
    @type printatoms: Iterable
    """
    p_init = open('printatomsinitfile','w')
    for atom in printatoms:
        print >>p_init, atom
    p_init.close()


# I'd be surprised if this were needed again
# Fri Sep 19 13:43:56 BST 2008
##def logllhds(self,filename):
##    """Since newmaxwalksat only computes integer costs
##    it is necessary to go through the file it creates
##    and assign the correct likelihood

##    @param filename: Name of file created by run of newmaxwalksat
##    @type filename: String
##    """
##    from os import rename
##    from math import exp
##    tmpname = 'deleteme'
##    tmp = open(tmpname,'w')
##    fobj = open(filename)
##    maxlogprob = 0
##    ints = fobj.readline().rstrip().split()[2:]
##    for family in ints:
##        ch, pas = self._wcnf_info['family_atoms'][int(family)]
##        maxlogprob += self._family_scores[ch][pas]
##    print >>tmp, 1.0, ' '.join(ints)
##    z = 1.0
##    for line in fobj:
##        logprob = 0
##        ints = line.rstrip().split()[2:]
##        for family in ints:
##            ch, pas = self._wcnf_info['family_atoms'][int(family)]
##            logprob += self._family_scores[ch][pas]
##        prob = exp(logprob - maxlogprob)
##        print >>tmp, prob, ' '.join(ints)
##        z += prob
##    tmp.close()
##    rename(tmpname,filename)
##    return z, maxlogprob

##def find_feasible(self,infilename,outfilename,n=1):
##    """Go through a list of digraphs encoding with arrows and
##    select the first n acyclic ones
##    """
##    print 'Filtering to ', outfilename
##    fobj = open(outfilename,'w')
##    tmp = {}
##    for (parent,child), atom in self._wcnf_info['pa_atom'].items():
##        tmp[str(atom)] = parent,child
##    num_bns = 1
##    for line in open(infilename):
##        fields = line.split()
##        cost = fields[1]
##        bn = ADG(self._variables)
##        for atom in fields[3:]:
##            arrow = tmp[atom]
##            try:
##                bn.add_arrow(arrow[0],arrow[1])
##            except DirectedCycleError:
##                break
##        else:
##            if num_bns == 1:
##                print cost, repr(bn)
##            print >>fobj, cost, repr(bn)
##            if num_bns >= n:
##                return
##            else:
##                num_bns += 1

##def find_feasible2(self,infilename,outfilename,n=1):
##    """Go through a list of digraphs encoding with
##    family atom and  select the first n acyclic ones
##    """
##    print 'Filtering to ', outfilename
##    fobj = open(outfilename,'w')
##    family_atoms = self._wcnf_info['family_atoms'] 
##    num_bns = 1
##    for line in open(infilename):
##        fields = line.split()
##        cost = fields[1]
##        bn = ADG(self._variables)
##        for atom in fields[3:]:
##            child,parents = family_atoms[int(atom)]
##            try:
##                bn.put_family(child,parents)
##            except DirectedCycleError:
##                break
##        else:
##            if num_bns == 1:
##                print cost, repr(bn)
##            print >>fobj, cost, repr(bn)
##            if num_bns >= n:
##                return
##            else:
##                num_bns += 1
