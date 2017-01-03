/*
 * Source code for Listing 9.6
 * 
 */

package mia.clustering.ch09;

import java.util.ArrayList;
import java.util.List;

import org.apache.mahout.clustering.fuzzykmeans.FuzzyKMeansClusterer;
import org.apache.mahout.clustering.fuzzykmeans.SoftCluster;
import org.apache.mahout.common.distance.EuclideanDistanceMeasure;
import org.apache.mahout.math.Vector;

public class FuzzyKMeansExample {

	public static void main(String[] args) {
		List<Vector> sampleData = new ArrayList<Vector>();

		RandomPointsUtil.generateSamples(sampleData, 400, 1, 1, 3);
		RandomPointsUtil.generateSamples(sampleData, 300, 1, 0, 0.5);
		RandomPointsUtil.generateSamples(sampleData, 300, 0, 2, 0.1);

		int k = 3;
		List<Vector> randomPoints = RandomPointsUtil.chooseRandomPoints(sampleData,	k);
		List<SoftCluster> clusters = new ArrayList<SoftCluster>();

		int clusterId = 0;
		for (Vector v : randomPoints) {
			clusters.add(new SoftCluster(v, clusterId++, new EuclideanDistanceMeasure()));
		}

		List<List<SoftCluster>> finalClusters = FuzzyKMeansClusterer
				.clusterPoints(sampleData, clusters,
						new EuclideanDistanceMeasure(), 0.01, 3, 10);
		for (SoftCluster cluster : finalClusters.get(finalClusters.size() - 1)) {
			System.out.println("Fuzzy Cluster id: " + cluster.getId()
					+ " center: " + cluster.getCenter().asFormatString());
		}
	}

}
