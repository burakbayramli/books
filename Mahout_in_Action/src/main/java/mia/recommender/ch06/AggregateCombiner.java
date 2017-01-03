/*
 * Source code for Listing 6.8
 * 
 */
package mia.recommender.ch06;

import java.io.IOException;

import org.apache.hadoop.mapreduce.Reducer;
import org.apache.mahout.math.VarLongWritable;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

public class AggregateCombiner
		extends
		Reducer<VarLongWritable, VectorWritable, VarLongWritable, VectorWritable> {

	public void reduce(VarLongWritable key, Iterable<VectorWritable> values,
			Context context) throws IOException, InterruptedException {
		Vector partial = null;
		for (VectorWritable vectorWritable : values) {
			partial = partial == null ? vectorWritable.get() : partial
					.plus(vectorWritable.get());
		}
		context.write(key, new VectorWritable(partial));
	}
}
