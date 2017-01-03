package mia.classifier.ch15;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.mahout.classifier.ConfusionMatrix;

public class ConfusionMatrixExample {
	public static void main(String[] args) throws IOException {
		String inputFile = args[1];
		
		BufferedReader in = new BufferedReader(new FileReader(inputFile)); 
		List<String> symbols = new ArrayList<String>();
		String line = in.readLine();
		while (line != null) {
		  String[] pieces = line.split(",");
		  if (!symbols.contains(pieces[0])) {
		    symbols.add(pieces[0]);
		  }
		  line = in.readLine();
		}

		ConfusionMatrix x2 = new ConfusionMatrix(symbols, "unknown");

		in = new BufferedReader(new FileReader(inputFile)); 
		line = in.readLine();
		while (line != null) {
		  String[] pieces = line.split(",");         
		  String trueValue = pieces[0];
		  String estimatedValue = pieces[1];
		  x2.addInstance(trueValue, estimatedValue);      
		  line = in.readLine();
		}
		System.out.printf("%s\n\n", x2.toString());
	}
}
