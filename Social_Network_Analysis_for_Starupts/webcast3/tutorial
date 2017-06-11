import networkx as net
import process_tweets

retweets=process_tweets.g

len(retweets)

retweets.remove_edges_from(retweets.selfloop_edges())
undir=net.to_networkx_graph(retweets)
core=net.k_core(undir)

len(core)

net.draw(core)

