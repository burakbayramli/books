import tweepy

class MyStreamListener(tweepy.StreamListener):
    def on_error(self, status_code):
        print 'An error has occured! Status code %s.' % status_code
        return True # keep stream alive

    def on_timeout(self):
        print 'Snoozing Zzzzzz'
        time.sleep(10)
        return True

    def on_delete(self, status_id, user_id):
        """Called when a delete notice arrives for a status"""
        #print "Delete notice for %s. %s" % (status_id, user_id)
        return

    def on_limit(self, track):
        """Called when a limitation notice arrvies"""
        print "!!! Limitation notice received: %s" % str(track)
        return
        
    def on_status(self, status):
        process_tweet(status)
    	   return True # or False if you want the stream to disconnect


def start_stream(username, password, listener, follow=(), track=():
	'''	
         follow: list of users to follow
		track: list of keywords to track
    '''
	print 'Connecting as %s/%s' % (username, password)
	stream = tweepy.Stream(username, password, listener, timeout=60)
	if follow or track:
		print "Starting filter on %s/%s" % (','.join(follow), ','.join(track))
		stream.filter(follow=follow, track=track, async=True)
	else:
		print "Starting sample"
		stream.sample(async=True)

# Process a sample stream:

listener = MyStreamListener()
start_stream("myusername","mypassword",listener)