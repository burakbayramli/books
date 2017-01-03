/*
 * Source code for Listing 6.7
 * 
 * This is simplified version of org.apache.mahout.cf.taste.hadoop.item.PartialMultiplyMapper class
 */
package mia.recommender.ch06;

import java.io.IOException;
import java.util.List;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.mahout.cf.taste.hadoop.item.VectorAndPrefsWritable;
import org.apache.mahout.math.VarLongWritable;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

public class PartialMultiplyMapper
		extends
		Mapper<IntWritable, VectorAndPrefsWritable, VarLongWritable, VectorWritable> {
	public void map(IntWritable key,
			VectorAndPrefsWritable vectorAndPrefsWritable, Context context)
			throws IOException, InterruptedException {
		Vector cooccurrenceColumn = vectorAndPrefsWritable.getVector();
		List<Long> userIDs = vectorAndPrefsWritable.getUserIDs();
		List<Float> prefValues = vectorAndPrefsWritable.getValues();
		
		for (int i = 0; i < userIDs.size(); i++) {
			long userID = userIDs.get(i);
			float prefValue = prefValues.get(i);
			Vector partialProduct = cooccurrenceColumn.times(prefValue);
			context.write(new VarLongWritable(userID), 
					new VectorWritable(partialProduct));
		}
	}
}
