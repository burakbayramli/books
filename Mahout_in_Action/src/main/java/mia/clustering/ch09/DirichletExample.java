/*
 * Source code for Listing 9.8
 * 
 */
package mia.clustering.ch09;

import java.util.ArrayList;
import java.util.List;

import org.apache.mahout.clustering.Cluster;
import org.apache.mahout.clustering.dirichlet.DirichletClusterer;
import org.apache.mahout.clustering.dirichlet.models.GaussianClusterDistribution;
import org.apache.mahout.math.DenseVector;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

public class DirichletExample {

	public static void main(String[] args) {
		List<Vector> sampleData = new ArrayList<Vector>();

		RandomPointsUtil.generateSamples(sampleData, 400, 1, 1, 3);
		RandomPointsUtil.generateSamples(sampleData, 300, 1, 0, 0.5);
		RandomPointsUtil.generateSamples(sampleData, 300, 0, 2, 0.1);

		List<VectorWritable> points = new ArrayList<VectorWritable>();
		for (Vector sd : sampleData) {
			points.add(new VectorWritable(sd));
		}

		DirichletClusterer dc = new DirichletClusterer(points,
				new GaussianClusterDistribution(new VectorWritable(
						new DenseVector(2))), 1.0, 10, 2, 2);
		List<Cluster[]> result = dc.cluster(20);
		for (Cluster cluster : result.get(result.size() - 1)) {
			System.out.println("Cluster id: " + cluster.getId() + " center: "
					+ cluster.getCenter().asFormatString());
		}
	}
}
