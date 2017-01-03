package mia.clustering.ch09;

import java.util.ArrayList;
import java.util.List;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.SequenceFile;
import org.apache.hadoop.io.Text;
import org.apache.mahout.clustering.canopy.Canopy;
import org.apache.mahout.clustering.canopy.CanopyClusterer;
import org.apache.mahout.clustering.kmeans.Cluster;
import org.apache.mahout.common.distance.CosineDistanceMeasure;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

public class CanopyClustering {
  
  public static void main(String args[]) throws Exception {
    
    String inputDir = "reuters";
    
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
    List<Canopy> canopies = CanopyClusterer.createCanopies(points, new CosineDistanceMeasure(), 0.8, 0.7);
    List<Cluster> clusters = new ArrayList<Cluster>();
    System.out.println(canopies.size());
    for (Canopy canopy : canopies) {
      clusters.add(new Cluster(canopy.getCenter(), canopy.getId(), new CosineDistanceMeasure()));
    }
  }
}
