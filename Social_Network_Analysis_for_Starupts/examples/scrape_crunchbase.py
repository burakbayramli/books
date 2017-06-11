import networkx as net

import requests
import simplejson as json

from collections import deque

comp=net.Graph()
base_url='http://api.crunchbase.com/v/1/company/'
ext='.js'
company='facebook'

q=deque()
visited=[]
q.append(company)

while len(q)>0:
    firm=q.popleft()
    url=base_url+firm+ext
    visited.append(firm)
    
    print firm, url 
    resp=requests.get(url)
    data=json.loads(resp.content)


    for c in data['competitions']:
        comp.add_edge(firm,c['competitor']['name'])
        if c['competitor']['name'] not in visited:
            q.append(c['competitor']['permalink'])
    