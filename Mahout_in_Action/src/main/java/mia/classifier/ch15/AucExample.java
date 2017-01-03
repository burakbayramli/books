package mia.classifier.ch15;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.apache.mahout.classifier.evaluation.Auc;
import org.apache.mahout.math.stats.GlobalOnlineAuc;
import org.apache.mahout.math.stats.OnlineAuc;

public class AucExample {

	public static void main(String[] args) throws IOException {
		String inputFile = args[1];
		
		Auc x1 = new Auc();
		OnlineAuc x2 = new GlobalOnlineAuc();
		BufferedReader in = new BufferedReader(new FileReader(inputFile));
		int lineCount = 0;
		String line = in.readLine();
		while (line != null) {
		  lineCount++;
		  String[] pieces = line.split(",");
		  double score = Double.parseDouble(pieces[0]);
		  int target = Integer.parseInt(pieces[1]);
		  x1.add(target, score);
		  x2.addSample(target, score);
		  if (lineCount%500 == 0) {
		    System.out.printf("%10d\t%10.3f\t%10d\t%.3f\n", 
		               lineCount, score, target, x2.auc());
		  } 
		  line = in.readLine();
		}

		System.out.printf("%d lines read\n", lineCount);
		System.out.printf("%10.2f = batch estimate\n", x1.auc());
		System.out.printf("%10.2f = on-line estimate\n", x2.auc());	
		}
}
