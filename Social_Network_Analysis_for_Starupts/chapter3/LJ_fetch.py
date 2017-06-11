#!/usr/bin/env python
# encoding: utf-8
"""
LJ_fetch.py

Created by Maksim Tsvetovat on 2011-04-28.
Copyright (c) 2011 __MyCompanyName__. All rights reserved.
"""

import sys
import os
import networkx as net
import urllib


def read_lj_friends(g, name):
    # fetch the friend-list from LiveJournal
    response=urllib.urlopen('http://www.livejournal.com/misc/fdata.bml?user='+name)
    for line in response.readlines():
        #Comments in the response start with a '#'
        if line.startswith('#'): continue 
        
        # the format is "< name" (incoming) or "> name" (outgoing)
        parts=line.split()
        
        #make sure that we don't have an empty line
        if len(parts)==0: continue
        
        #add the edge to the network
        if parts[0]=='<': 
            g.add_edge(parts[1],name)
        else:
            g.add_edge(name,parts[1])

def snowball_sampling(g, center, max_depth=1, current_depth=0, taboo_list=[]):
    # if we have reached the depth limit of the search, bomb out.
    print center, current_depth, max_depth, taboo_list
    if current_depth==max_depth: 
        print 'out of depth'
        return taboo_list
    if center in taboo_list:
        print 'taboo' 
        return taboo_list #we've been here before
    else:
        taboo_list.append(center) # we shall never return
        
    read_lj_friends(g, center)
    
    for node in g.neighbors(center):
        taboo_list=snowball_sampling(g, node, current_depth=current_depth+1, max_depth=max_depth, taboo_list=taboo_list)
    
    return taboo_list
    
    







def main():
    g=net.Graph()
#    read_lj_friends(g,'kozel_na_sakse')
    snowball_sampling(g,'kozel_na_sakse')
    

if __name__ == '__main__':
    main()

