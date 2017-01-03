package mia.classifier.ch14;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import org.apache.lucene.util.Version;
import org.apache.mahout.classifier.sgd.L1;
import org.apache.mahout.classifier.sgd.OnlineLogisticRegression;
import org.apache.mahout.math.DenseVector;
import org.apache.mahout.math.RandomAccessSparseVector;
import org.apache.mahout.math.Vector;
import org.apache.mahout.vectorizer.encoders.ConstantValueEncoder;
import org.apache.mahout.vectorizer.encoders.FeatureVectorEncoder;
import org.apache.mahout.vectorizer.encoders.StaticWordValueEncoder;
import org.apache.mahout.vectorizer.encoders.Dictionary;

import com.google.common.collect.ConcurrentHashMultiset;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.Iterables;
import com.google.common.collect.Multiset;

public class TrainNewsGroups {
	private static final int FEATURES = 10000;
	private static Multiset<String> overallCounts;
	
	public static void main(String[] args) {
		File base = new File(args[0]);
		overallCounts = HashMultiset.create();

		Map<String, Set<Integer>> traceDictionary = new TreeMap<String, Set<Integer>>();
		FeatureVectorEncoder encoder = new StaticWordValueEncoder("body");
		encoder.setProbes(2);
		encoder.setTraceDictionary(traceDictionary);
		FeatureVectorEncoder bias = new ConstantValueEncoder("Intercept");
		bias.setTraceDictionary(traceDictionary);
		FeatureVectorEncoder lines = new ConstantValueEncoder("Lines");
		lines.setTraceDictionary(traceDictionary);
		Dictionary newsGroups = new Dictionary();
		
		OnlineLogisticRegression learningAlgorithm = 
		    new OnlineLogisticRegression(
		          20, FEATURES, new L1())
		        .alpha(1).stepOffset(1000)
		        .decayExponent(0.9) 
		        .lambda(3.0e-5)
		        .learningRate(20);
		
		List<File> files = new ArrayList<File>();
		for (File newsgroup : base.listFiles()) {
		  newsGroups.intern(newsgroup.getName());
		  files.addAll(Arrays.asList(newsgroup.listFiles()));
		}

		Collections.shuffle(files);
		System.out.printf("%d training files\n", files.size());
		
		double averageLL = 0.0;
		double averageCorrect = 0.0;
		double averageLineCount = 0.0;
		int k = 0;
		double step = 0.0;
		int[] bumps = new int[]{1, 2, 5};
		double lineCount = 0;
		
		Analyzer analyzer = new StandardAnalyzer(Version.LUCENE_31);
		
/*		for (File file : files) {
			  BufferedReader reader = new BufferedReader(new FileReader(file));
			  String ng = file.getParentFile().getName();     
			  int actual = newsGroups.intern(ng);
			  Multiset<String> words = ConcurrentHashMultiset.create();
			  
			  String line = reader.readLine();
			  while (line != null && line.length() > 0) {
			    if (line.startsWith("Lines:")) {              
			      String count = Iterables.get(onColon.split(line), 1);
			      try {
			        lineCount = Integer.parseInt(count);
			        averageLineCount += (lineCount - averageLineCount) 
			            / Math.min(k + 1, 1000);
			      } catch (NumberFormatException e) {
			        lineCount = averageLineCount;
			      }
			    }
			    boolean countHeader = (
			        line.startsWith("From:") || line.startsWith("Subject:")||
			        line.startsWith("Keywords:")|| line.startsWith("Summary:"));
			    do {
			      StringReader in = new StringReader(line);
			      if (countHeader) {
			        countWords(analyzer, words, in);    
			      }
			      line = reader.readLine();
			    } while (line.startsWith(" "));
			  }
			  countWords(analyzer, words, reader);      
			  reader.close();
			}
		
		Vector v = new RandomAccessSparseVector(FEATURES);
		bias.addToVector(null, 1, v);
		lines.addToVector(null, lineCount / 30, v);
		logLines.addToVector(null, Math.log(lineCount + 1), v);
		for (String word : words.elementSet()) {
		  encoder.addToVector(word, Math.log(1 + words.count(word)), v);
		}
		*/
		
		/*double mu = Math.min(k + 1, 200);
		double ll = learningAlgorithm.logLikelihood(actual, v);  #1
		averageLL = averageLL + (ll - averageLL) / mu;

		Vector p = new DenseVector(20);
		learningAlgorithm.classifyFull(p, v);
		int estimated = p.maxValueIndex();

		int correct = (estimated == actual? 1 : 0);
		averageCorrect = averageCorrect + (correct - averageCorrect) / mu;*/
		
/*		learningAlgorithm.train(actual, v);
		k++;
		int bump = bumps[(int) Math.floor(step) % bumps.length];
		int scale = (int) Math.pow(10, Math.floor(step / bumps.length));
		if (k % (bump * scale) == 0) {
		  step += 0.25;
		  System.out.printf("%10d %10.3f %10.3f %10.2f %s %s\n",
		       k, ll, averageLL, averageCorrect * 100, ng, 
		       newsGroups.values().get(estimated));
		}
		learningAlgorithm.close();*/
		
	}

	private static void countWords(Analyzer analyzer, Collection<String> words, Reader in) throws IOException {
	    TokenStream ts = analyzer.tokenStream("text", in);
	    ts.addAttribute(CharTermAttribute.class);
	    while (ts.incrementToken()) {
	      String s = ts.getAttribute(CharTermAttribute.class).toString();
	      words.add(s);
	    }
	    /*overallCounts.addAll(words);*/
	  }
}
