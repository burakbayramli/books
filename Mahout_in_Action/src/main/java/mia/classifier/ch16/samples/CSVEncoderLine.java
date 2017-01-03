/*
 * Source code for Listing 16.1
 * 
 */
package mia.classifier.ch16.samples;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;


import org.apache.mahout.math.DenseVector;
import org.apache.mahout.math.Vector;
import org.apache.mahout.vectorizer.encoders.ConstantValueEncoder;
import org.apache.mahout.vectorizer.encoders.FeatureVectorEncoder;

public class CSVEncoderLine {

	private static final int FIELDS = 10;

	public static void main(String[] args) throws IOException {
		FeatureVectorEncoder[] encoder = new FeatureVectorEncoder[FIELDS];
		for (int i = 0; i < FIELDS; i++) {
			encoder[i] = new ConstantValueEncoder("v" + i);
		}
		long t0 = System.currentTimeMillis();
		Vector v = new DenseVector(1000);
		BufferedReader in = new BufferedReader(new FileReader(args[1]));
		String line = in.readLine();
		while (line != null) {
			v.assign(0);
			Line x = new Line(line);
			for (int i = 0; i < FIELDS; i++) {
				encoder[i].addToVector((byte[]) null, x.getDouble(i), v);
			}
			line = in.readLine();
		}
		System.out.printf("\nElapsed time = %.3f s\n",
				(System.currentTimeMillis() - t0) / 1000.0);
	}

}
