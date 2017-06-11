import networkx as net
import urllib
import json

cp=net.DiGraph() #company to person
ci=net.DiGraph() #company to investor
cc=net.DiGraph() #company to company -- competitors


def get_company(cp,ci,cc,name):

name='twitter'    
response=urllib.urlopen('http://api.crunchbase.com/v/1/company/'+name+'.js')
s=""
for l in response.readlines() : s=s+l
js=json.loads(s)
print len(s), len(js), len(js['relationships'])
##get the list of employees, competitors and investor
for e in js['relationships']:
    person=e['person']['permalink']
    cp.add_edge(name,person)
    
for c in js['competitions']:
    company = c['competitor']['permalink']
    cc.add_edge(name,company)
    
for round in js['funding_rounds']:
    for i in round['investments']:
        investor=i['financial_org']
        if investor != None:    
            ci.add_edge(name,investor['permalink'])
    