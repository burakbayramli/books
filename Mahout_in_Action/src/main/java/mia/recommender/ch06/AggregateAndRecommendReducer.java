/*
 * Source code for Listing 6.9
 * 
 * This is simplified version of org.apache.mahout.cf.taste.hadoop.item.AggregateAndRecommendReducer class
 * 
 */
package mia.recommender.ch06;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.mahout.cf.taste.hadoop.RecommendedItemsWritable;
import org.apache.mahout.cf.taste.hadoop.TasteHadoopUtils;
import org.apache.mahout.cf.taste.impl.recommender.ByValueRecommendedItemComparator;
import org.apache.mahout.cf.taste.impl.recommender.GenericRecommendedItem;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;
import org.apache.mahout.math.VarLongWritable;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;
import org.apache.mahout.math.map.OpenIntLongHashMap;

public class AggregateAndRecommendReducer
		extends
		Reducer<VarLongWritable, VectorWritable, VarLongWritable, RecommendedItemsWritable> {

	private int recommendationsPerUser = 10;
	private OpenIntLongHashMap indexItemIDMap;
	static final String ITEMID_INDEX_PATH = "itemIDIndexPath";
	static final String NUM_RECOMMENDATIONS = "numRecommendations";
	static final int DEFAULT_NUM_RECOMMENDATIONS = 10;

	protected void setup(Context context) throws IOException {
		Configuration jobConf = context.getConfiguration();
		recommendationsPerUser = jobConf.getInt(NUM_RECOMMENDATIONS,
				DEFAULT_NUM_RECOMMENDATIONS);
		indexItemIDMap = TasteHadoopUtils.readItemIDIndexMap(
				jobConf.get(ITEMID_INDEX_PATH), jobConf);
	}

	public void reduce(VarLongWritable key, Iterable<VectorWritable> values,
			Context context) throws IOException, InterruptedException {

		Vector recommendationVector = null;
		for (VectorWritable vectorWritable : values) {
			recommendationVector = recommendationVector == null ? vectorWritable
					.get() : recommendationVector.plus(vectorWritable.get());
		}

		Queue<RecommendedItem> topItems = new PriorityQueue<RecommendedItem>(
				recommendationsPerUser + 1,
				Collections.reverseOrder(ByValueRecommendedItemComparator
						.getInstance()));

		Iterator<Vector.Element> recommendationVectorIterator = recommendationVector
				.iterateNonZero();
		while (recommendationVectorIterator.hasNext()) {
			Vector.Element element = recommendationVectorIterator.next();
			int index = element.index();
			float value = (float) element.get();
			if (topItems.size() < recommendationsPerUser) {
				topItems.add(new GenericRecommendedItem(indexItemIDMap
						.get(index), value));
			} else if (value > topItems.peek().getValue()) {
				topItems.add(new GenericRecommendedItem(indexItemIDMap
						.get(index), value));
				topItems.poll();
			}
		}

		List<RecommendedItem> recommendations = new ArrayList<RecommendedItem>(
				topItems.size());
		recommendations.addAll(topItems);
		Collections.sort(recommendations,
				ByValueRecommendedItemComparator.getInstance());
		context.write(key, new RecommendedItemsWritable(recommendations));
	}
}
