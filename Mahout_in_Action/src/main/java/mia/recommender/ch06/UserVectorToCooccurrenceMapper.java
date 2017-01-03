/*
 * Source code for Listing 6.3
 * 
 */
package mia.recommender.ch06;

import java.io.IOException;
import java.util.Iterator;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.mahout.math.VarLongWritable;
import org.apache.mahout.math.VectorWritable;
import org.apache.mahout.math.Vector;

public class UserVectorToCooccurrenceMapper extends
		Mapper<VarLongWritable, VectorWritable, IntWritable, IntWritable> {

	public void map(VarLongWritable userID, VectorWritable userVector,
			Context context) throws IOException, InterruptedException {
		Iterator<Vector.Element> it = userVector.get().iterateNonZero();
		while (it.hasNext()) {
			int index1 = it.next().index();
			Iterator<Vector.Element> it2 = userVector.get().iterateNonZero();
			while (it2.hasNext()) {
				int index2 = it2.next().index();
				context.write(new IntWritable(index1), new IntWritable(index2));
			}
		}
	}
}
