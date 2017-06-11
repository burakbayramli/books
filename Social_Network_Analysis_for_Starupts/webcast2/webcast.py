import networkx as net
import matplotlib.pyplot as plot
import math
import csv 

file="retweets.txt"

g=net.Graph() #create a blank graph

reader=csv.reader(open(file,'rb'),delimiter=' ')
for line in reader:
    g.add_edge(line[0],line[1],weight=int(line[2]))
    

"""    
    In [17]: len(g)
    Out[17]: 129461
"""

components=net.connected_component_subgraphs(g)

"""
In [21]: len(components)
Out[21]: 17202
"""

l=[len(c) for c in components]

"""
In [24]: l[:10]
Out[24]: 
[65044,
 204,
 103,
 81,
 77,
 72,
 71,
 68,
 64,
 60]
 
net.draw(components[1])
"""

g1=components[0]
degree=net.degree(g1)

weights=[edata['weight'] for f,t,edata in g1.edges(data=True)]
hist=plot.hist(weights,100)


def trim_edges(g, weight=1):
    """
    Remove edges with weights less then a threshold parameter ("weight")
    """
    g2=net.Graph()
    for f, to, edata in g.edges(data=True):
        if edata['weight'] > weight:
            g2.add_edge(f,to,edata)
    return g2

"""
In [74]: g2=trim_edges(g1)
In [75]: len(g2)
Out[75]: 24657

In [78]: g2=trim_edges(g1, weight=2)
In [79]: len(g2)
Out[79]: 16451
....
In [82]: g2=trim_edges(g1, weight=10)
In [84]: len(g2)
Out[84]: 3357

In [91]: g3=net.connected_component_subgraphs(g2)[0]
In [92]: len(g3)
Out[92]: 1461
"""

degree=net.degree(g3)
pos=net.spring_layout(g3)
ns=[degree[n]*100 for n in g3.nodes()]
net.draw_networkx(g3,pos=pos,node_size=ns,with_labels=False)

def sorted_degree(g):
    d=net.degree(g)
    ds = sorted(d.iteritems(), key=lambda (k,v): (-v,k))
    return ds
    


"""
In [127]: ds=sorted_degree(g3)

In [128]: ds[:10]
Out[128]: 
[('alarabiya_ar', 97),
 ('AJArabic', 50),
 ('Shorouk_News', 44),
 ('Ghonim', 35),
 ('AJEnglish', 34),
 ('AlArabiya_Eng', 33),
 ('AymanM', 30),
 ('monaeltahawy', 26),
 ('EANewsFeed', 21),
 ('HGhazaryan', 20)]
"""

def sorted_map(dct):
    ds = sorted(dct.iteritems(), key=lambda (k,v): (-v,k))
    return ds
    
btw=net.betweenness_centrality(g3)
ns=[btw[n]*1000+10 for n in g3.nodes()]
net.draw_networkx(g3,pos=pos,node_size=ns,with_labels=False)

bs=sorted_map(btw)
bs[:10]

"""
[('Ghonim', 0.30948683318099868),
 ('alarabiya_ar', 0.22141480094013333),
 ('monaeltahawy', 0.21103708928780715),
 ('shary20', 0.21082511576960963),
 ('dadlani', 0.16175663741677401),
 ('AJArabic', 0.13889259139494639),
 ('Reza_Kahlili', 0.12907330369968981),
 ('CFHeather', 0.12113052931463912),
 ('WSJ', 0.12103851658308157),
 ('PERSIA_MAX_NEWS', 0.12016294134886295)]"""

pr=net.pagerank(g3)
ns=[pr[n]*1000+10 for n in g3.nodes()]
net.draw_networkx(g3,pos=pos,node_size=ns,with_labels=False)

prs=sorted_map(btw)
prs[:10]

"""
[('Ghonim', 0.30948683318099868),
 ('alarabiya_ar', 0.22141480094013333),
 ('monaeltahawy', 0.21103708928780715),
 ('shary20', 0.21082511576960963),
 ('dadlani', 0.16175663741677401),
 ('AJArabic', 0.13889259139494639),
 ('Reza_Kahlili', 0.12907330369968981),
 ('CFHeather', 0.12113052931463912),
 ('WSJ', 0.12103851658308157),
 ('PERSIA_MAX_NEWS', 0.12016294134886295)]

"""
