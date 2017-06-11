import tweetstream
from webcast import *
import networkx as net


words = ["Obama", "Romney", "republican","democrat","election"]
##people = [123,124,125]
#locations = ["-122.75,36.8", "-121.75,37.8"] #, follow=people, locations=locations


retweets=net.DiGraph()
hashtag_net=net.Graph()
spatial=net.Graph()

import geocoder
geo = geocoder.geocoder()

with tweetstream.FilterStream("<your user ID>", "<password>", track=words) as stream:
	for js in stream:
		
	### process tweet to extract information
		try:
			author=js['user']['screen_name']
			entities=js['entities']
			mentions=entities['user_mentions']
			hashtags=entities['hashtags']
			location=geo.geocode(js)

			for rt in mentions:
				alter=rt['screen_name']
				retweets.add_edge(author,alter)

			tags=[tag['text'].lower() for tag in hashtags]
			for t1 in tags: 
				if location is not None and 'city' in location:
					spatial.add_node(location['city'],type='location',lat=location['latitude'],lon=location['longitude'])
					add_or_inc_edge(spatial,t1,location['city'])
					
				for t2 in tags:
					if t1 is not t2:
						add_or_inc_edge(hashtag_net,t1,t2)      
		except :
			print ':-('
			continue


