/*
 * To run this example you need to obtain auth keys from https://dev.twitter.com/apps
 * and enter them into corresponding .setOAuthXXXX methods in the main function below
 * 
 */
package mia.clustering.ch12;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import twitter4j.Status;
import twitter4j.StatusDeletionNotice;
import twitter4j.StatusListener;
import twitter4j.TwitterStream;
import twitter4j.TwitterStreamFactory;
import twitter4j.conf.ConfigurationBuilder;

public class TwitterDownloader {
	private class MahoutListener implements StatusListener {
    	long count = 0;
    	static final long maxCount = 100000; // change this if you want another number of tweets to collect
    	PrintWriter out;
    	TwitterStream tweetStream;
    	
    	MahoutListener (TwitterStream ts) throws IOException {
    		tweetStream = ts;
    		out = new PrintWriter(new BufferedWriter(new FileWriter("tweets.txt")));
    	}
    	
        public void onStatus(Status status) {
        	String username = status.getUser().getScreenName();
        	String text = status.getText().replace('\n', ' ');
            out.println(username + "\t" + text);
            System.out.println(username + "\t" + text);
            count++;
            if(count >= maxCount) {
            	tweetStream.shutdown();
            	out.close();
            }
        }

        public void onDeletionNotice(StatusDeletionNotice statusDeletionNotice) {
        }

        public void onTrackLimitationNotice(int numberOfLimitedStatuses) {
        }

        public void onScrubGeo(long userId, long upToStatusId) {
        }

        public void onException(Exception ex) {
            ex.printStackTrace();
        }
	}
	
	StatusListener makeListener(TwitterStream ts) throws IOException {
		return this.new MahoutListener(ts);
	}
	
	public static void main(String[] args) throws IOException {
		ConfigurationBuilder cb = new ConfigurationBuilder();
		cb.setDebugEnabled(true)
		  .setOAuthConsumerKey("*********")
		  .setOAuthConsumerSecret("************")
		  .setOAuthAccessToken("*******************")
		  .setOAuthAccessTokenSecret("****************");
		TwitterStream twitterStream = new TwitterStreamFactory(cb.build()).getInstance();
        TwitterDownloader td = new TwitterDownloader();
        StatusListener listener = td.makeListener(twitterStream);
        twitterStream.addListener(listener);
        twitterStream.sample();
	}

}
