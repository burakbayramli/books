                               
>>> len(retweets)
>>> net.draw(retweets)
>>> undir_retweets=retweets.to_undirected()
>>> comps=net.connected_component_subgraphs(undir_retweets)
>>> len(comps)
>>> len(comps[0])
>>> net.draw(comps[0])

degrees=net.degree(comps[0])

degrees=sorted_degree(comps[0])
degrees[:10] 

plot.hist(net.degree(comps[0]).values(),50)

core=trim_degrees(comps[0])

len(core)
2836

len(hashtag_net)
1753

net.draw(hashtag_net)

core=net.connected_component_subgraphs(hashtag_net)[0]
net.draw(core)

core.remove_node('earthquake')
core2=trim_edges(hashtag_net, weight=2)
net.draw(core2)

core3=trim_edges(hashtag_net, weight=10)
net.draw(core3)