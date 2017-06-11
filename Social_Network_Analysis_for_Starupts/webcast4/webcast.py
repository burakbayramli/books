import networkx as net
from networkx.algorithms import bipartite as bi

from collections import Counter

candidates={}
can_file=open('2012/foiacn.txt','rb')
for line in can_file:
    cid=line[0:9]
    name=line[9:47].strip()
    party=line[47:50]
    inc=line[56]
    zip=line[147:152]
    candidates[cid]={'name':name,'party':party,'type':ctype,'inc':inc,'zip':zip}

[(k,pacs[k]) for k in pacs.keys()[:10]]

ctype_counter=Counter()
for can in candidates.values():
    ctype_counter[can['type']]+=1
ctype_counter

from pac_types import pac_types
pacs={}
pac_file=open('2012/foiacm.txt','rb')
for line in pac_file:
    pid=line[0:9]
    ctype=line[0]
    name=line[9:99].strip()
    party=line[232-235]
    ctype=line[231]
    zip=line[225:230]
    pacs[pid]={'name':name,'party':party,'type':ctype,'zip':zip}

ctype_counter=Counter()
for pac in pacs.values():
    ctype_counter[pac['type']]+=1
ctype_counter


g=net.Graph()
can_list=[]
pac_list=[]
contrib=open('2012/itpas2.txt','rb')
for line in contrib:
    pid=line[0:9]
    cid=line[52:61]
    #amt=int(line[36:43])
    g.add_edge(pid,cid)
    if cid not in can_list: can_list.append(cid)
    if pid not in pac_list: pac_list.append(pid)
    if pid in pacs: 
        g.node[pid]=pacs[pid]
    else:
        pacs[pid]={'type':'unknown'}
    if cid in candidates: 
        g.node[cid]=candidates[cid]
    else:
        candidates[cid]={'type':'unknown'}
        

cannet=bi.weighted_projected_graph(g, can_list, ratio=False)

def trim_edges(g, weight=1):
	g2=net.Graph()
	for f, to, edata in g.edges(data=True):
		if edata['weight'] > weight:
			g2.add_edge(f,to,edata)
			g2.node[f]=g.node[f]
			g2.node[to]=g.node[to]
	return g2

import multimode as mm

cancore=trim_edges(cannet, weight=50)
mm.plot_multimode(cancore, type_string='party')

pacnet=bi.weighted_projected_graph(g, pac_list, ratio=False)
paccore = trim_edges(pacnet, weight=50)


def sorted_map(dct):
    ds = sorted(dct.iteritems(), key=lambda (k,v): (-v,k))
    return ds

d=sorted_map(net.degree(paccore))
c=sorted_map(net.closeness_centrality(paccore))
inf_pacs=[pacs[pid] for pid,deg in d[:10]]
close_pacs=[pacs[pid] for pid,deg in c[:10]]


"""
[{'name': 'NATIONAL ASSOCIATION OF REALTORS POLITICAL ACTION COMMITTEE',
  'party': ' ',
  'type': 'Q',
  'zip': '60611'},
 {'name': 'AT&T INC. FEDERAL POLITICAL ACTION COMMITTEE (AT&T FEDERAL PAC)',
  'party': ' ',
  'type': 'Q',
  'zip': '75202'},
 {'name': 'UNITED PARCEL SERVICE INC. PAC',
  'party': ' ',
  'type': 'Q',
  'zip': '30328'},
 {'name': 'HONEYWELL INTERNATIONAL POLITICAL ACTION COMMITTEE',
  'party': ' ',
  'type': 'Q',
  'zip': '20001'},
 {'name': "LOCKHEED MARTIN CORPORATION EMPLOYEES' POLITICAL ACTION COMMITTEE",
  'party': ' ',
  'type': 'Q',
  'zip': '22202'},
 {'name': 'NATIONAL BEER WHOLESALERS ASSOCIATION POLITICAL ACTION COMMITTEE',
  'party': ' ',
  'type': 'Q',
  'zip': '22314'},
 {'name': 'GENERAL ELECTRIC COMPANY POLITICAL ACTION COMMITTEE (GEPAC)',
  'party': ' ',
  'type': 'Q',
  'zip': '20004'},
 {'name': 'COMCAST CORPORATION POLITICAL ACTION COMMITTEE- FEDERAL',
  'party': ' ',
  'type': 'Q',
  'zip': '19103'},
 {'name': 'THE BOEING COMPANY POLITICAL ACTION COMMITTEE',
  'party': ' ',
  'type': 'Q',
  'zip': '22209'},
 {'name': 'VERIZON COMMUNICATIONS INC./VERIZON WIRELESS GOOD GOVERNMENT CLUB (VERIZON/VERIZON WIRELES',
  'party': ' ',
  'type': 'Q',
  'zip': '20005'}]
  """
