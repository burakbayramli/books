import tweepy

# First, the basics

"""
Consumer key 	wADh1LqyQCR3OmEGqK3SDg
Consumer secret 	FzKWL6bMfL6oHvHwh9daANHuSScXua5K386513FbU6c
Request token URL 	https://api.twitter.com/oauth/request_token
Authorize URL 	https://api.twitter.com/oauth/authorize
Access token URL 	https://api.twitter.com/oauth/access_token
Access token 	153439378-AuXJgQ8oHmnY0JSabav6kGNoVg5iOB7t9CF3B3cF
Access token secret 	LKm3AlD0fhCE4ofZXYZALxtsMNBaRqXmJWiTgUT1Jlo
"""

access_token='153439378-AuXJgQ8oHmnY0JSabav6kGNoVg5iOB7t9CF3B3cF'
access_token_secret='LKm3AlD0fhCE4ofZXYZALxtsMNBaRqXmJWiTgUT1Jlo'


def connect():
	auth = tweepy.OAuthHandler("myAuthToken",access_token)
	auth.set_access_token("myAccessToken", access_token_secret)
	api = tweepy.API(auth)
	if api and api.verity_credentials():
		return api
	else:
		print("Login failed.")



query = '"someScreenName" OR "#sometag"' # a valid Twitter search query

def run_search(query = query):
	q = {
		'q': query,
		'lang': 'en',
	}
	
	api = connect()
	try:
		for status in Cursor(api.search, **q).items():
			process_tweet(status)
	except TweepError:
		traceback.print_exc()
		raise