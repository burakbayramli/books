"""Example objects (BNs, FRs, etc) and demos

@var asia: The famous 'Asia' Bayesian net.
This object was constructed from the 'Asia.dnet' file in the Netica library of BNs
@type asia: L{BN}
@var train: Small BN used to introduce conditional independence
@type train: L{BN}
@var contab: A small contingency table with just the variables:
'Smoking', 'Cancer' and 'Bronchitis'
@type contab: L{Factor}
@var minibn: A subnet of L{asia} with just the variables:
'Smoking', 'Cancer' and 'Bronchitis'
@type minibn: L{BN}
@var nondecomp: An unnormalised 4-variable non-decomposable fitted hierarchical model
@type nondecomp: L{FR}
@var nondecomp_norm: A normalised version of L{nondecomp}
@type nondecomp_norm: L{FR}
@var card1: A univariate factor representing choosing a playing card uniformly.
@type card1: L{Factor}
@var card2: A univariate factor representing choosing a playing card uniformly.
@type card2: L{Factor}
@var indep_hm: A distribution with 3 independent binary variables
@type indep_hm: L{FR}
@var tarjan1: 10 vertex graph from Tarjan and Yannakakis

  Refs::

         @Article{tarjan84:_simpl,
         author = 	 {Robert E. Tarjan and Mihalis Yannakakis},
         title = 	 {Simple linear-time algorithms to test chordality of graphs, test acyclicity of hypergraphs, and selectively reduce acyclic hypergraphs}, 
         journal = 	 {{SIAM} Journal of Computing},
         year = 	 1984,
         volume =	 13,
         number =	 3,
         pages =	 {566--579},
         month =	 {August}
         }
@type tarjan1: L{UGraph}
@var berryfig6: 10 vertex graph from Berry et al

 Refs::
   @Article{berry04:_maxim_cardin_searc_comput_minim_trian_graph,
   author = 	 {Anne Berry and Jean Blair and Pinar Heggernes and Barry Peytton},
   title = 	 {Maximum Cardinality Search for Computing Minimal Triangulations of Graphs},
   journal = 	 {Algorithmica},
   year = 	 2004,
   volume =	 39,
   number =	 4,
   pages =	 {287--298}
   } 
@type berryfig6: L{UGraph}
@var cancerdat: String representing a file representing a simple
record of counts
@type cancerdat: String
@var floridadat: String representing a file representing a simple
record of counts for Florida death penalty example
@type floridadat: String
@var _f1: A factor
@type _f1: L{Factor}
@var _f2: A factor
@type _f2: L{Factor}
@var _f3: A factor
@type _f3: L{Factor}
@var _f4: A factor
@type _f4: L{Factor}
@var _twmap: Maps twlib graph names to their numbers
@type _twmap: Dictionary
@var _version: Version of this module
@type _version: String
"""

from Models import BN, FR
from Parameters import Factor, CPT
from Hypergraphs import Hypergraph, ReducedGraphicalHypergraph
from Graphs import ADG, UGraph
from IO import read_twlib

_version = '$Id: Examples.py,v 1.3 2008/10/07 09:09:01 jc Exp $'

train=BN([
    CPT(Factor(['LeaveOnTime'],[0.4,0.6],new_domain_variables={'LeaveOnTime':['n','y']}),child='LeaveOnTime'),
    CPT(Factor(['LeaveOnTime','CatchTrain'],[0.8,0.2,0.2,0.8],new_domain_variables={'CatchTrain':['n','y']}),child='CatchTrain'),
    CPT(Factor(['CatchTrain','WorkOnTime'],[0.7,0.3,0.1,0.9],new_domain_variables={'WorkOnTime':['n','y']}),child='WorkOnTime')])

asia=BN([
    CPT(Factor(frozenset(['XRay', 'TbOrCa']),[0.050000000000000003, 0.94999999999999996, 0.97999999999999998, 0.02],new_domain_variables={'Cancer': frozenset(['absent', 'present']), 'Bronchitis': frozenset(['absent', 'present']), 'TbOrCa': frozenset(['true', 'false']), 'XRay': frozenset(['abnormal', 'normal']), 'VisitAsia': frozenset(['no_visit', 'visit']), 'Tuberculosis': frozenset(['absent', 'present']), 'Smoking': frozenset(['smoker', 'nonsmoker']), 'Dyspnea': frozenset(['absent', 'present'])}),child='XRay'),
    CPT(Factor(frozenset(['VisitAsia']),[0.98999999999999999, 0.01]),child='VisitAsia'),
    CPT(Factor(frozenset(['VisitAsia', 'Tuberculosis']),[0.98999999999999999, 0.94999999999999996, 0.01, 0.050000000000000003]),child='Tuberculosis'),
    CPT(Factor(frozenset(['Smoking', 'Bronchitis']),[0.69999999999999996, 0.40000000000000002, 0.29999999999999999, 0.59999999999999998]),child='Bronchitis'),
    CPT(Factor(frozenset(['Smoking']),[0.5, 0.5]),child='Smoking'),
    CPT(Factor(frozenset(['TbOrCa', 'Bronchitis', 'Dyspnea']),[0.90000000000000002, 0.29999999999999999, 0.10000000000000001, 0.69999999999999996, 0.20000000000000001, 0.10000000000000001, 0.80000000000000004, 0.90000000000000002]),child='Dyspnea'),
    CPT(Factor(frozenset(['Smoking', 'Cancer']),[0.98999999999999999, 0.90000000000000002, 0.01, 0.10000000000000001]),child='Cancer'),
    CPT(Factor(frozenset(['TbOrCa', 'Tuberculosis', 'Cancer']),[1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0]),child='TbOrCa')],
         adg=ADG(
    ['Bronchitis', 'Cancer', 'Dyspnea', 'Smoking', 'TbOrCa', 'Tuberculosis', 'VisitAsia', 'XRay'],
    [('Bronchitis', 'Dyspnea'), ('Cancer', 'TbOrCa'), ('Smoking', 'Bronchitis'), ('Smoking', 'Cancer'), ('TbOrCa', 'Dyspnea'), ('TbOrCa', 'XRay'), ('Tuberculosis', 'TbOrCa'), ('VisitAsia', 'Tuberculosis')],
    {'Cancer': [185.99999999999997, 97.0], 'Bronchitis': [298.99999999999994, 131.0], 'TbOrCa': [138.0, 149.0], 'XRay': [80.0, 195.99999999999997], 'VisitAsia': [57.0, 37.0], 'Tuberculosis': [110.0, 82.0], 'Smoking': [253.0, 52.0], 'Dyspnea': [257.0, 213.0]}))

contab = Factor(frozenset(['Smoking', 'Cancer', 'Bronchitis']),[3, 0, 27, 15, 2, 0, 18, 35],None,{'Smoking': frozenset(['smoker', 'nonsmoker']), 'Cancer': frozenset(['absent', 'present']), 'Bronchitis': frozenset(['absent', 'present'])})

minibn = BN((asia['Smoking'],asia['Cancer'],asia['Bronchitis'])) 

_f1 = Factor(('A','B'),[0.1,0.2,0.3,0.2],new_domain_variables={'A':['0','1'],'B':['0','1']},check=True)
_f2 = Factor(('B','C'),[0.4,0.7,0.3,0.1],new_domain_variables={'C':['0','1']},check=True)
_f3 = Factor(('C','D'),[0.5,0.2,0.4,0.1],new_domain_variables={'D':['0','1']},check=True)
_f4 = Factor(('A','D'),[0.9,0.2,0.7,0.1],check=True)
nondecomp = FR((_f1,_f2,_f3,_f4))
nondecomp_norm = FR((_f1/nondecomp.z(),_f2,_f3,_f4))

card1 = Factor(['Card1'],new_domain_variables={'Card1':range(52)}).normalised()
card2 = Factor(['Card2'],new_domain_variables={'Card2':range(52)}).normalised()

indep_hm = FR(
    [Factor(['X1'],[1,9]),Factor(['X2'],[4,6]),Factor(['X3'],[7,3])],
    new_domain_variables={'X1':['0','1'], 'X2':['0','1'], 'X3':['0','1']})

tarjan1 = UGraph(range(1,11),((1,2),(1,3),(2,3),(2,10),(3,10),(4,5),
                        (4,7),(5,6),(5,9),(5,7),(6,7),(6,9),
                        (7,8),(7,9),(8,9),(8,10),(9,10)),
                 {10: (173.0, 145.0), 1: (180.0, 221.99999999999997), 3: (226.99999999999997, 176.99999999999997), 2: (114.0, 176.99999999999997), 5: (231.99999999999997, 30.0), 4: (112.99999999999999, 30.0), 7: (112.99999999999999, 65.0), 6: (195.0, 70.0), 9: (234.0, 112.99999999999999), 8: (112.0, 112.0)})

berryfig6 = UGraph(range(1,11),((1,3),(1,10),(3,2),(3,4),(3,8),(2,6),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)))

cancerdat = """Smoker:smoker,nonsmoker
Cancer:present,absent
Bronchitis:present,absent
Bronchitis,Cancer,Smoker
absent,absent,nonsmoker,35
absent,absent,smoker,18
absent,present,nonsmoker,0
absent,present,smoker,2
present,absent,nonsmoker,15
present,absent,smoker,27
present,present,nonsmoker,0
present,present,smoker,03
"""

floridadat = """Victim:black,white
Murderer:black,white
Sentence:death,other
Victim,Murderer,Sentence
black,black,death,11
black,black,other,2309
black,white,death,0
black,white,other,111
white,black,death,48
white,black,other,238
white,white,death,72
white,white,other,2074
"""

def factor_mult():
    """Demo showing two univariate factors being multiplied"""
    from gPy.Demos import prod_gui
    cancer = asia['Smoking'] * asia['Cancer']
    cancer = cancer.sumout(['Smoking'])
    visit = asia['VisitAsia'] * 1
    prod_gui(visit,cancer)

factor_mult_demo = factor_mult

def prod_gen():
    """Demo showing factors being multiplied"""
    from gPy.Demos import prod_gui

    f1 = asia['Bronchitis'] * asia['Cancer']
    f2 = asia['Bronchitis'] * 1
    f3 = asia['Bronchitis'] * asia['Dyspnea']
    f4 = asia['TbOrCa'] * 1

    prod_gui(f1,f2)
    prod_gui(f1,f3)
    prod_gui(f1,f4)

prod_gen_demo = prod_gen

def norming():
    """Demo showing computation and absorption of a 'Z' normalising constant"""
    from Tkinter import Tk, Label

    root = Tk()
    nondecomp.gui_display(root)
    Label(root,text='Z is %s' % nondecomp.z()).pack()
    nondecomp_norm.gui_display(root)
    root.mainloop()

norming_demo = norming

def compare_ves():
    """Demo showing time taken for variable elimination with 2 different
    elimination orders"""
    from timeit import Timer
    for order in (
        ['VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
         'Smoking', 'TbOrCa'],
        ['TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
         'Smoking', 'Dyspnea']):
        veti = Timer("asia.copy(True).variable_elimination(%s)" % order,
                     "from gPy.Examples import asia")
        print 'Time taken for 1000 iterations of variable elimination using order %s:' % order
        print veti.timeit(1000)

compare_ves_demo = compare_ves

def big_fr(n=1000,vals=50):
    """Return a factored representation with potentially many
    'overlapping' bivariate factors

    @param n: Number of factors in the distribution
    @type n: integer
    @param vals: Number of values for each variable (same for all)
    @type vals: integer
    @return: Factored representation with n+1 variables and n factors
    @rtype: L{FR}
    """
    from Variables import Domain
    d = Domain()
    for x in range(n+1):
        d.add_domain_variable('X'+str(x),range(vals))
    big = FR(domain=d)
    for x in range(n):
        vs = ['X'+str(x),'X'+str(x+1)]
        big *= Factor(vs,[0.3]*(vals**2),domain=d)
    return big
        

def compare_ve_conds(n=100,vals=10,rpts=10):
    """Runs a comparison of naive and non-naive variable elimination
    when there's conditioning of a factored representation produced by L{big_fr}

    @param n: Number of factors in the distribution
    @type n: integer
    @param vals: Number of values for each variable (same for all)
    @type vals: integer
    @param rpts: Number of repeats of variable elimination
    @type rpts: integer
    """
    from timeit import Timer
    vs = ['X'+str(n) for n in range(1,n+1)]
    ts = []
    for naive in (True,False):
        veti = Timer("fr.copy(True).condition({'X1':[0]}).variable_elimination(%s,%s)" % (
            vs,naive),"from gPy.Examples import big_fr; fr = big_fr(%s,%s)" % (n,vals))
        print 'Time taken with naive = %s:' % naive
        t = veti.timeit(rpts)
        print t
        ts.append(t)
    ratio = ts[0]/ts[1]
    print 'Speed up', ratio

compare_ve_conds_demo = compare_ve_conds

def compare_ve_conds2():
    """Runs a comparison of naive and non-naive variable elimination
    when there's conditioning"""
    from timeit import Timer
    for naive in (True,False):
        veti = Timer("asia.copy(True).condition({'Cancer':['absent'],'TbOrCa':['false']}).variable_elimination(['VisitAsia','Tuberculosis', 'XRay','Dyspnea', 'Bronchitis', 'Smoking','TbOrCa'],%s)" % naive,
                     "from gPy.Examples import asia")
        print 'Time taken with naive = %s:' % naive
        print veti.timeit(1000)

compare_ve_conds2_demo = compare_ve_conds2

def ve_demo():
    """Demo displaying how variable elimination works"""
    _ve_demo(asia.copy(True))

def ve_demo2():
    """Demo displaying how variable elimination works (with conditioning)"""
    tmp = asia.copy(True)
    tmp.condition({'Bronchitis':['absent'],'XRay':['normal']})
    _ve_demo(tmp)

ve2_demo = ve_demo2

def _ve_demo(asia):
    from Tkinter import Tk, Label, Frame, Button
    from gPy.Utils import scrolled_frame
    for cpt in asia.copy(True):
        cpt *= 1
    root = Tk()
    top=scrolled_frame(root,yscroll=8000, height=40000) 
    windows = (Frame(top),Frame(top))
    orders =(('VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
              'Smoking', 'TbOrCa'),
             ('TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
              'Smoking', 'Dyspnea'))
    asias = (asia.copy(True),asia.copy(True))
    for i in range(2):
        model, window, order = asias[i], windows[i], orders[i]
        window.pack()
        for variable in order:
            cpm = model.copy()
            step = model.eliminate_variable(variable,trace=True)
            prod_factor, message, hyperedges = step
            colours = {}
            for hyperedge in hyperedges:
                colours[hyperedge] = 'blue'
            Label(window,text = 'Eliminating '+ variable).pack() 
            cpm.gui_display(window,colours)
            if message is not None:
                fr = Frame(window)
                fr.pack()
                model[message].gui_main(fr,edit=False,bg='red').pack()
                Button(fr,text='Remove message',command=fr.destroy).pack()
    root.mainloop()

def bad(model,n):
    """Return hypergraphs from Badsberg's thesis

    Refs::
    
     @PhdThesis{badsberg95:_envir_graph_model,
     author = 	 {Badsberg, Jens Henrik},
     title = 	 {An Environment for Graphical Models},
     school = 	 {University of Aarhus},
     year = 	 1995
     }
     """
    
    letter, indx = model[0].lower(), int(model[1])
    model_dict = {
        'd': [None, _bad_d1, _bad_d2, _bad_d3, _bad_d4],
        'g': [None, _bad_g1, _bad_g2, _bad_g3, _bad_g4]
        }
    return model_dict[letter][indx](n)

def _bad_d1(n):
    return Hypergraph([[x] for x in range(1,n+1)])

def _bad_d2(n):
    return Hypergraph([range(1,n+1)])

def _bad_d3(n):
    hs = set()
    for i in range(1,n+1):
        if i > 2:
            hs.add(frozenset([i-2,i-1,i]))
    return Hypergraph(hs)

def _bad_d4(n):
    if n > 1:
        return Hypergraph([range(1,n),range(2,n+1)])
    else:
        return Hypergraph([range(2,n+1)])

def _bad_g1(n):
    hs = set()
    for i in range(1,n+1):
        if i > 4:
            hs.add(frozenset([i-4,i-3,i-2]))
    hs.update([frozenset([n-3,n-1]),
               frozenset([n-2,n]),
               frozenset([n-1,n])
               ])
    return Hypergraph(hs)

def _bad_g2(n):
    hs = set()
    for i in range(1,n+1):
        tmp = [[i-2,i-1],[i-3,i],[i-1,i]]
        if i < 4:
            new_hs = _nolessthanone(tmp)
        else:
            new_hs = tmp
        for h in new_hs:
            hs.add(frozenset(h))
    return Hypergraph(hs)


def _bad_g3(n):
    hs = set()
    for i in range(1,n+1):
        tmp = [[i-5,i-4],[i-5,i-2],[i-4,i-3],[i-3,i-2],[i-1,i]]
        if i < 6:
            new_hs = _nolessthanone(tmp)
        else:
            new_hs = tmp
        for h in new_hs:
            hs.add(frozenset(h))
    hs.update([frozenset([n,1]),frozenset([n-2,1])])
    return Hypergraph(hs)

def _bad_g4(n):
    hs = set()
    for i in range(1,n+1):
        tmp = [[i-23,i-22,i-21,i-20],[1,i-19],[i-20,i-18],[i-19,i-18,i-15],
               [i-18,i-17],[i-17,i-16],[i-16,i-15],
               [i-15,i-14,i-13,i-12],[1,i-11],[i-12,i-10],[i-15,i-12,i-11],
               [i-10,i-7],[i-10,i-9],[i-9,i-8],[i-8,i-7],
               [i-7,i-6,i-5,i-4],[1,i-3],[i-4,i-2],[i-3]] # UNFINISHED
        if i < 6:
            new_hs = _nolessthanone(tmp)
        else:
            new_hs = tmp
        for h in new_hs:
            hs.add(frozenset(h))
    hs.update([frozenset([n,1]),frozenset([n-2,1])])
    return Hypergraph(hs)


def _nolessthanone(tmp):
    new_hs = []
    for h in tmp:
        if h[0] > 0:
            new_hs.append(h)
    return new_hs


def time_mc():
    """Times maximum cardinality search on various random graphs""" 
    from timeit import Timer
    for ratio in (0.2,0.4,0.6,0.8):
        for n in (10,20,40,80,100):
            m = n*n*ratio
            print n+m, Timer(
                'g.maximum_cardinality_search()' ,
                'from gPy.Graphs import UGraph; g=UGraph().random_graph(%d,%d)' % (n,m)
                ).timeit(100)
        print

time_mc_demo = time_mc

def time_chk():
    """Demo comparing checking for a zero fill-in with and without early exiting
    from maximum cardinality search"""
    # times quite similar if graphs not completed
    from timeit import Timer
    for n in (50,100,150,200,250):
        print (n,
               Timer('g.zero_fillin_check()' ,
                     'from gPy.Graphs import UGraph; g=UGraph(range(%d)); g.complete(g.vertices())' % n
                     ).timeit(10),
               Timer('g.is_triangulated()' ,
                     'from gPy.Graphs import UGraph; g=UGraph(range(%d)); g.complete(g.vertices())' % n
                     ).timeit(10))
        print

time_chk_demo = time_chk

def time_fillin():
    """Demo comparing times for naive vs Tarjan-Yannakakis fill-in algorithms"""
    from timeit import Timer
    for ratio in (0.2,0.4,0.6,0.8):
        for n in (10,20,40,80,100):
            m = n*n*ratio
            print (n+m,
                   Timer('g.triangulate2(g.vertices())' ,
                         'from gPy.Graphs import UGraph; g=UGraph().random_graph(%d,%d)' % (n,m)
                         ).timeit(10),
                   Timer('g.triangulate(g.vertices(),modify=True)' ,
                         'from gPy.Graphs import UGraph; g=UGraph().random_graph(%d,%d)' % (n,m)
                         ).timeit(10))
        print

time_fillin_demo = time_fillin

def time_hgs():
    """Demo comparing Graham's algorithm to maximum cardinality search to
    check for hypergraph decomposability"""
    from timeit import Timer
    for m in ('d1','d2','d3','d4','g1','g2','g3','g4'):
        print m
        for n in (10,20,40,80,100,1000):
            print (n,
                   Timer('hg.copy().grahams()' ,
                         'from gPy.Examples import bad; hg=bad("%s",%d)' % (m,n)
                         ).timeit(10),
                   Timer('hg.copy().is_decomposable()' ,
                         'from gPy.Examples import bad; hg=bad("%s",%d)' % (m,n)
                         ).timeit(10))
        print

time_hgs_demo = time_hgs

def igs():
    """Demo which just displays two 2-section graphs for two hypergraphs"""
    from Tkinter import Tk, Label
    root=Tk()
    nondecomp_norm.interaction_graph().gui_display(root)
    Label(root,text=str(nondecomp_norm.hypergraph())).pack()
    minibn.interaction_graph().gui_display(root)
    Label(root,text=str(minibn.hypergraph())).pack()
    root.mainloop()

igs_demo = igs

def igs2():
    """Demo which displays the interaction graph (which is also the moralised graph)
    for the Asia BN, and its associated hypergraph"""
    from Tkinter import Tk, Label

    root=Tk()
    asia.interaction_graph().gui_display(root)
    Label(root,text=str(asia.hypergraph())).pack()
    root.mainloop()

igs2_demo = igs2

def grahams_demo(**config):
    """Demo of Graham's algorithm"""
    gr = asia.adg().moralise()
    gr.add_line('Cancer','Bronchitis')
    general_grahams_demo(gr.hypergraph(),**config)

def grahams2_demo(**config):
    """Demo of Graham's algorithm"""
    gr = asia.adg().moralise()
    general_grahams_demo(gr.hypergraph(),**config)

grahams_demo2 = grahams2_demo

def grahams3_demo(**config):
    """Demo of Graham's algorithm"""
    general_grahams_demo(
        Hypergraph([[3,4],[2,4],[1,2,3],[2,3,4]]),
        **config)

grahams_demo3 = grahams3_demo

def grahams4_demo(**config):
    """Demo of Graham's algorithm"""
    general_grahams_demo(
        hg = Hypergraph([[3,4],[2,4],[1,2,3]]),
        **config)

grahams_demo4 = grahams4_demo

def general_grahams_demo(hg,width=400,height=300,
                 scrolled_width=1000,scrolled_height=40000,
                 xscroll=1000,yscroll=1000,**config):
    """Demo of Graham's algorithm"""
    from Tkinter import Tk
    from gPy.Utils import scrolled_frame
    root=scrolled_frame(Tk(),width=scrolled_width,height=scrolled_height,
                        xscroll=xscroll,yscroll=yscroll)
    hg.gui_grahams_algorithm(root,width=width,height=height,**config)
    root.mainloop()


def triangulation_demo():
    """Demo showing triangulation of the Asia moral graph with a good and a bad
    elimination ordering"""
    from Tkinter import Tk
    root = Tk()
    root.title("Triangulation demonstration")
    orders =(['VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
              'Smoking', 'TbOrCa','Cancer'],
             ['TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
              'Smoking', 'Dyspnea','Cancer'])
    ig = asia.adg().moralise()
    for order in orders:
        ig.copy().gui_triangulate(root,order)
    root.mainloop()


def max_card_search_demo(width=400,height=300,
                 scrolled_width=1000,scrolled_height=40000,
                 xscroll=1000,yscroll=1000,**config):
    """Demo of maximum cardinality search on the Asia moral graph and on
    L{tarjan1}."""
    from Tkinter import Tk
    root1 = Tk()
    from gPy.Utils import scrolled_frame
    root=scrolled_frame(root1,width=scrolled_width,height=scrolled_height,
                        xscroll=xscroll,yscroll=yscroll)
    root1.title("Maximum cardinality search demonstration")
    asia.adg().moralise().gui_maximum_cardinality_search(root,
                                                         width=width,
                                                         height=height,
                                                         **config)
    def m(set):
        x = max(set)
        set.remove(x)
        return x
    tarjan1.gui_maximum_cardinality_search(root,m,
                                           width=width,
                                           height=height,
                                           **config)
    root.mainloop()


def tw_graph(i):
    """Return a graph from Treewidthlib

    @param i: Number of desired graph
    @type i: integer
    @return: Graph number i from Treewidthlib
    @rtype: L{UGraph}
    """
    vertices, edges = read_twlib(i)
    return UGraph(vertices,edges)

_twmap = {
    'myciel3': 314,
    'myciel4': 315,
    'barley-pp' : 142,
    'celar03-pp-01' : 132,
    'fungiuk': 260,
    'mainuk': 261,
    'mildew': 253,
    'oesoca+-pp': 144,
    'pathfinder-pp': 137,
    'queen5-5': 176,
    'queen6-6': 177,
    'queen7-7': 178,
    'water': 215,
    'water-pp': 216,
    'weeduk': 262}

def test_mcs():
    """not ready for use, will be used to test the efficacy of MCS
    for finding elimination orderings"""
    for key, val in _twmap.items():
        print key,
        g = tw_graph(val)
        print len(g.vertices()), len(g.edges()),
        nh = g.hypergraph().msc_decompcover()
        l=0
        for x in nh:
            if len(x) > l:
                l=len(x)
        print l-1
    
def cancer_table():
    """Prints out a small contingency table and its normalised version"""
    from IO import read_csv
    from Data import CompactFactor
    import Parameters
    from StringIO import StringIO

    cancer = CompactFactor(read_csv(StringIO(cancerdat)))
    # create a normal factor
    table = cancer['Smoker', 'Cancer', 'Bronchitis']
    print table
    print 'Number of observations is %d' % table.z()
    Parameters.precision = 6
    print table.normalised()

def marginalise_demo():
    """Generates a GUI to demo marginalisation on a small contingency table"""
    from IO import read_csv
    from Data import CompactFactor
    from Demos import marginalise_gui
    from StringIO import StringIO

    cancer = CompactFactor(read_csv(StringIO(cancerdat)))
    # create a normal factor
    data = cancer['Smoker', 'Cancer', 'Bronchitis']
    marginalise_gui(data)

def marginalise_demo2():
    """Generates a GUI to demo marginalisation on a small joint distribution"""
    from IO import read_csv
    from Data import CompactFactor
    from Demos import marginalise_gui
    from StringIO import StringIO

    cancer = CompactFactor(read_csv(StringIO(cancerdat)))
    # create a normal factor
    data = cancer['Smoker', 'Cancer', 'Bronchitis']
    marginalise_gui(data.normalised())

marginalise2_demo = marginalise_demo2

def prod_demo():
    """Demo of factor multiplication of univariate factors"""
    from Demos import prod_gui

    cancer = asia['Smoking'] * asia['Cancer']
    cancer = cancer.sumout(['Smoking'])
    visit = asia['VisitAsia'] * 1
    prod_gui(visit,cancer)

def prod_gen():
    """Demo of general factor multiplication"""
    from Demos import prod_gui

    f1 = asia['Bronchitis'] * asia['Cancer']
    f2 = asia['Bronchitis'] * 1
    f3 = asia['Bronchitis'] * asia['Dyspnea']
    f4 = asia['TbOrCa'] * 1

    prod_gui(f1,f2)
    prod_gui(f1,f3)
    prod_gui(f1,f4)
    
def cond():
    """Demo showing CPTs as quotients of factors"""
    from Demos import div_gui

    div_gui(minibn['Bronchitis'] * minibn['Smoking'],minibn['Smoking'])
    div_gui(minibn['Cancer'] * minibn['Smoking'],minibn['Smoking'])

def redund():
    """Display examples of redundant factors"""
    from Tkinter import Tk

    root = Tk()
    for cpt in minibn:
        cpt *= 1
    minibn.gui_display(root)
    minibn.red()
    minibn.gui_display(root)
    root.mainloop()

redund_demo = redund

def cluster_tree():
    """Displays cluster trees for 2 elimination orders for Asia BN"""
    from Tkinter import Toplevel, Label
    from Utils import scrolled_frame, pretty_str_set
    top=scrolled_frame(Toplevel(),yscroll=500, height=400)
    bottom=Toplevel()
    for cpt in asia:
        cpt *= 1
    asia.gui_display(top)
    order1 = ('VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
              'Smoking', 'TbOrCa')
    order2 = ('TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
              'Smoking', 'Dyspnea')
    for order in (order1, order2):
        ac = asia.copy(True)
        cf = ac.variable_elimination_trace(order)
        cf.gui_display(bottom,pp_vertex=pretty_str_set,width=600,height=300)
        Label(bottom,text=order).pack()
    top.mainloop()
    bottom.mainloop()

cluster_tree_demo = cluster_tree

def cond_demo():
    """Show conditioning by deleting instantiations"""
    from Tkinter import Tk
    from gPy.Utils import scrolled_frame
    as2 = asia.copy(True)
    for cpt in as2:
        cpt *= 1
    root = Tk()
    top=scrolled_frame(root,yscroll=5000, height=40000)
    as2.gui_display(top)
    as2.condition({'Smoking':['smoker']})
    as2.gui_display(top)
    root.mainloop()

def asia_cpts():
    """Shows Asia BN: CPTs, DAG, CPTs as factors and moral graph"""
    from Tkinter import Tk
    from Utils import scrolled_frame
    root=Tk()
    top=scrolled_frame(root,yscroll=5000, height=4000)
    as2 = asia.copy(True)
    as2.gui_display(top)
    as2.adg().gui_display(top,width=400,height=300)
    for factor in as2:
        factor *= 1
    as2.gui_display(top)
    as2.adg().moralise().gui_display(top,width=400,height=300)
    root.mainloop()

asia_cpts_demo = asia_cpts

def join_forest_demo():
    """Display 2 join forests for Asia for 2 different variable orderings"""
    from Tkinter import Toplevel, Label
    from Utils import scrolled_frame, pretty_str_set
    from Hypergraphs import JoinForest
    top=scrolled_frame(Toplevel(),yscroll=500, height=400)
    bottom=scrolled_frame(Toplevel(),yscroll=500, height=400)
    as2 = asia.copy(True)
    for cpt in as2:
        cpt *= 1
    as2.gui_display(top)
    order1 = ('VisitAsia', 'Tuberculosis', 'XRay',  'Dyspnea', 'Bronchitis', 
              'Smoking', 'TbOrCa', 'Cancer')
    order2 = ('TbOrCa', 'VisitAsia', 'Tuberculosis', 'XRay', 'Bronchitis', 
              'Smoking', 'Dyspnea', 'Cancer')
    for order in (order1, order2):
        ac = as2.copy()
        jf = JoinForest(ac.hypergraph().make_decomposable2(order)[0])
        jf._uforest.gui_display(bottom,pp_vertex=pretty_str_set,width=600,height=300)
        Label(bottom,text=order).pack()
    top.mainloop()
    bottom.mainloop()

def forward_sample_demo():
    """Show estimation of probabilities by forward sampling"""
    from Samplers import BNSampler

    sampler = BNSampler(asia)
    cancer_index = sampler.variable_index('Cancer')
    xray_index = sampler.variable_index('XRay')

    for size in 10,100,1000,10000:
        print 'Sample size: ', size
        for j in range(5):
            sample = []
            for i in xrange(size):
                sample.append(sampler.forward_sample())
            cancer_count, both_count = 0, 0
            for inst in sample:
                if inst[cancer_index] == 'absent':
                    cancer_count += 1
                    if inst[xray_index] == 'abnormal':
                        both_count += 1
            print 'Estimate of P(Cancer=absent): ', cancer_count/float(size)
            print 'Estimate of P(Cancer=absent,XRay=abnormal): ', both_count/float(size)
            try:
                print 'Estimate of P(XRay=abnormal|Cancer=absent): ', both_count/float(cancer_count)
            except ZeroDivisionError:
                print '0/0!'
                print

def rejection_sample_demo():
    """Show estimation of probabilities by rejection sampling"""
    from Samplers import BNSampler

    sampler = BNSampler(asia)
    sampler.condition({'XRay':'abnormal'})
    print "Samples with 'None's are rejected" 
    for i in xrange(100):
        print sampler.rejection_sample()

def florida_demo():
    """Show Florida death penalty data"""
    from IO import read_csv
    from Data import CompactFactor
    import Parameters
    from StringIO import StringIO

    florida = CompactFactor(read_csv(StringIO(floridadat)))
    #create a normal factor
    table = florida['Murderer', 'Sentence', 'Victim']
    print table
    print 'Number of observations is %d' % table.z()
    Parameters.precision = 6
    print table.normalised()

def calibration_demo(width=400,height=300,
                 scrolled_width=40000,scrolled_height=40000,
                 xscroll=2000,yscroll=1000,**config):
    """Demo of join tree calibration"""
    from gPy.Models import JFR
    from Tkinter import Tk
    from gPy.Utils import scrolled_frame
    root=scrolled_frame(Tk(),width=scrolled_width,height=scrolled_height,
                        xscroll=xscroll,yscroll=yscroll)
    jfr = JFR(asia.copy(),modify=True,
              elimination_order=['VisitAsia','Tuberculosis','Smoking','Cancer',
                                 'TbOrCa','XRay','Bronchitis','Dyspnea'])
    jfr.gui_calibrate(root)
    root.mainloop()

