package com.manning.hip.testdata;

import edu.uci.ics.crawler4j.crawler.CrawlController;
import edu.uci.ics.crawler4j.crawler.Page;
import edu.uci.ics.crawler4j.crawler.WebCrawler;
import edu.uci.ics.crawler4j.url.WebURL;

import java.util.regex.Pattern;

public class Crawler {

		public static void main(String[] args) throws Exception {
			String rootFolder = "/tmp";
			int numberOfCrawlers = 1;

			CrawlController controller = new CrawlController(rootFolder);
			controller.addSeed("http://hadoop.apache.org/");
      controller.addSeed("http://hadoop.apache.org/common/");
      controller.addSeed("http://hadoop.apache.org/hdfs/");
      controller.addSeed("http://hadoop.apache.org/mapreduce/");
      controller.addSeed("http://avro.apache.org/");
      controller.addSeed("http://hbase.apache.org/");
      controller.addSeed("http://hive.apache.org/");
      controller.addSeed("http://pig.apache.org/");
      controller.addSeed("http://zookeeper.apache.org/");
			controller.setPolitenessDelay(1000);
			controller.setMaximumCrawlDepth(2);
			controller.setMaximumPagesToFetch(1);

			controller.start(MyCrawler.class, numberOfCrawlers);
		}


  public static class MyCrawler extends WebCrawler {

	Pattern filters = Pattern.compile(".*(\\.(css|js|bmp|gif|jpe?g"
			+ "|png|tiff?|mid|mp2|mp3|mp4" + "|wav|avi|mov|mpeg|ram|m4v|pdf"
			+ "|rm|smil|wmv|swf|wma|zip|rar|gz))$");

	public MyCrawler() {
	}

	public boolean shouldVisit(WebURL url) {
		String href = url.getURL().toLowerCase();
    return !filters.matcher(href).matches() &&
        href.contains("apache.org");
  }

	public void visit(Page page) {
        String url = page.getWebURL().getURL();

    // standard out contains a single line per URL, with the URL
    // followed by all the words found on the page
    //
    String text = page.getText().replaceAll("[^a-zA-Z]+", " ");
    System.out.println(url + "\t" + text);

    // standard err contains a line for each outgoing link from the
    // page we're crawling
    //
    for(WebURL link: page.getURLs()) {
      System.err.println(url + "\t" + link.getURL());
    }
	}
}

}