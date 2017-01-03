/*
 * Source code for Listing 6.4
 * 
 */
package mia.recommender.ch06;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.mahout.math.RandomAccessSparseVector;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

public class UserVectorToCooccurrenceReducer extends
		Reducer<IntWritable, IntWritable, IntWritable, VectorWritable> {

	public void reduce(IntWritable itemIndex1,
			Iterable<IntWritable> itemIndex2s, Context context)
			throws IOException, InterruptedException {
		Vector cooccurrenceRow = new RandomAccessSparseVector(
				Integer.MAX_VALUE, 100);
		for (IntWritable intWritable : itemIndex2s) {
			int itemIndex2 = intWritable.get();
			cooccurrenceRow.set(itemIndex2,
					cooccurrenceRow.get(itemIndex2) + 1.0);
		}
		context.write(itemIndex1, new VectorWritable(cooccurrenceRow));
	}
}
