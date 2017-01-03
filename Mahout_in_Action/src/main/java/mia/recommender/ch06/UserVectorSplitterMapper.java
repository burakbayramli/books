/*
 * Source code for Listing 6.6
 * 
 * This is simplified version of org.apache.mahout.cf.taste.hadoop.item.UserVectorSplitterMapper class
 */
package mia.recommender.ch06;

import java.io.IOException;
import java.util.Iterator;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.mahout.cf.taste.hadoop.item.VectorOrPrefWritable;
import org.apache.mahout.math.VarLongWritable;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

public class UserVectorSplitterMapper
		extends
		Mapper<VarLongWritable, VectorWritable, IntWritable, VectorOrPrefWritable> {

	public void map(VarLongWritable key, VectorWritable value, Context context)
			throws IOException, InterruptedException {
		long userID = key.get();
		Vector userVector = value.get();
		Iterator<Vector.Element> it = userVector.iterateNonZero();
		IntWritable itemIndexWritable = new IntWritable();
		while (it.hasNext()) {
			Vector.Element e = it.next();
			int itemIndex = e.index();
			float preferenceValue = (float) e.get();
			itemIndexWritable.set(itemIndex);
			context.write(itemIndexWritable, 
					new VectorOrPrefWritable(userID, preferenceValue));
		}
	}
}
