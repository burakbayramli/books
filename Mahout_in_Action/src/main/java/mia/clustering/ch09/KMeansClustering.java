package mia.clustering.ch09;

import java.util.ArrayList;
import java.util.List;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.SequenceFile;
import org.apache.hadoop.io.Text;
import org.apache.mahout.clustering.kmeans.Cluster;
import org.apache.mahout.clustering.kmeans.KMeansClusterer;
import org.apache.mahout.common.distance.CosineDistanceMeasure;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

public class KMeansClustering {
  
  public static void main(String args[]) throws Exception {
    
    String inputDir = "reuters";
    int k = 25;
    
    Configuration conf = new Configuration();
    FileSystem fs = FileSystem.get(conf);
    String vectorsFolder = inputDir + "/tfidf-vectors";
    SequenceFile.Reader reader = new SequenceFile.Reader(fs, new Path(vectorsFolder + "/part-r-00000"), conf);
    List<Vector> points = new ArrayList<Vector>();
    Text key = new Text();
    VectorWritable value = new VectorWritable();
    
    while (reader.next(key, value)) {
      points.add(value.get());
    }
    System.out.println(points.size());
    reader.close();
    List<Vector> randomPoints = RandomPointsUtil.chooseRandomPoints(points, k);
    List<Cluster> clusters = new ArrayList<Cluster>();
    System.out.println(randomPoints.size());
    int clusterId = 0;
    for (Vector v : randomPoints) {
      clusters.add(new Cluster(v, clusterId++, new CosineDistanceMeasure()));
    }
    
    List<List<Cluster>> finalClusters = KMeansClusterer.clusterPoints(points, clusters,
      new CosineDistanceMeasure(), 10, 0.01);
    for (Cluster cluster : finalClusters.get(finalClusters.size() - 1)) {
      System.out.println("Cluster id: " + cluster.getId() + " center: "
                         + cluster.getCenter().asFormatString());
    }
    
  }
}
