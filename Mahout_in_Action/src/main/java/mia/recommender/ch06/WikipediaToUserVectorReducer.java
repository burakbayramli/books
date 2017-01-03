/*
 * Source code for Listing 6.2
 * 
 */
package mia.recommender.ch06;

import java.io.IOException;

import org.apache.hadoop.mapreduce.Reducer;
import org.apache.mahout.math.RandomAccessSparseVector;
import org.apache.mahout.math.VarLongWritable;
import org.apache.mahout.math.VectorWritable;
import org.apache.mahout.math.Vector;

public class WikipediaToUserVectorReducer
		extends
		Reducer<VarLongWritable, VarLongWritable, VarLongWritable, VectorWritable> {

	public void reduce(VarLongWritable userID,
			Iterable<VarLongWritable> itemPrefs, Context context)
			throws IOException, InterruptedException {
		Vector userVector = new RandomAccessSparseVector(Integer.MAX_VALUE, 100);
		for (VarLongWritable itemPref : itemPrefs) {
			userVector.set((int) itemPref.get(), 1.0f);
		}
		context.write(userID, new VectorWritable(userVector));
	}
}
