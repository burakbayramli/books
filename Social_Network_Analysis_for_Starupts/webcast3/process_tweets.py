#!/usr/bin/env python
# encoding: utf-8
"""
reprocess.py

Created by Maksim Tsvetovat on 2012-02-15.
Copyright (c) 2012 __MyCompanyName__. All rights reserved.
"""

import sys
import os
import simplejson as json 
from dateutil import parser
import networkx as net


def add_or_inc_edge(g,f,t):
    """
    Adds an edge to the graph IF the edge does not exist already. 
    If it does exist, increment the edge weight.
    Used for quick-and-dirty calculation of projected graphs from 2-mode networks.
    """
    if g.has_edge(f,t):
        g[f][t]['weight']+=1
    else:
        g.add_edge(f,t,weight=1)



tweet_dir='tweet_data/'
filez=os.listdir(tweet_dir)
g=net.DiGraph()
for file in filez:
#file=filez[0]
    f_in=open(tweet_dir+file,'rb')
    
    print "<<<<"+file+">>>>>"
    ### each line in the file corresponds to 1 tweet in a raw format
    ### we will build retweet networks from the at-tags in the file
    for line in f_in:
        try:
            tweet=json.loads(line)
        except:
            ##some JSON records are malformed. Skip them
            continue
        
        ## harvest attags from the JSON structure; skip tweet if there is an error
        try:
            author=tweet['user']['screen_name']
            attags=tweet['entities']['user_mentions']
            ret_from=tweet['in_reply_to_screen_name']
        except:
            continue
        
        if ret_from:
            print author, ret_from
            add_or_inc_edge(g,author,ret_from)

        for attag in attags:
            print author, attag['screen_name']
            add_or_inc_edge(g,author,attag['screen_name'])
        
    
        print '.',
    print "@@@@@"