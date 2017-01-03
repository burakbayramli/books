package mia.recommender.ch06;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WikipediaDataConverter {
	private static final Pattern NUMBERS = Pattern.compile("(\\d+)");
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new FileReader("links-simple-sorted.txt"));
		PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter("links-converted.txt")));
		String line;
		  while ((line = in.readLine()) != null)   {
			Matcher m = NUMBERS.matcher(line);
			m.find();
			long userID = Long.parseLong(m.group());
			while (m.find()) {
				out.println(userID + "," + Long.parseLong(m.group()));
			}
		  }
		  in.close();
		  out.close();
	}

}
