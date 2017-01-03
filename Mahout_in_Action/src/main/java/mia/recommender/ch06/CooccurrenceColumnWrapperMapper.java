/*
 * Source code for Listing 6.5
 * 
 */
package mia.recommender.ch06;

import java.io.IOException;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.mahout.cf.taste.hadoop.item.VectorOrPrefWritable;
import org.apache.mahout.math.VectorWritable;

public class CooccurrenceColumnWrapperMapper extends
		Mapper<IntWritable, VectorWritable, IntWritable, VectorOrPrefWritable> {

	public void map(IntWritable key, VectorWritable value, Context context)
			throws IOException, InterruptedException {
		context.write(key, new VectorOrPrefWritable(value.get()));
	}
}
