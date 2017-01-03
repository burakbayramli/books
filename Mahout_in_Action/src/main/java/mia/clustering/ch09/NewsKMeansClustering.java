/*
 * Source code for Listing 9.4
 * 
 */

package mia.clustering.ch09;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.SequenceFile;
import org.apache.lucene.analysis.Analyzer;
import org.apache.mahout.clustering.Cluster;
import org.apache.mahout.clustering.WeightedVectorWritable;
import org.apache.mahout.clustering.canopy.CanopyDriver;
import org.apache.mahout.clustering.kmeans.KMeansDriver;
import org.apache.mahout.common.HadoopUtil;
import org.apache.mahout.common.distance.EuclideanDistanceMeasure;
import org.apache.mahout.common.distance.TanimotoDistanceMeasure;
import org.apache.mahout.vectorizer.DictionaryVectorizer;
import org.apache.mahout.vectorizer.DocumentProcessor;
import org.apache.mahout.vectorizer.tfidf.TFIDFConverter;

public class NewsKMeansClustering {
  
  public static void main(String args[]) throws Exception {
    
    int minSupport = 5;
    int minDf = 5;
    int maxDFPercent = 95;
    int maxNGramSize = 2;
    int minLLRValue = 50;
    int reduceTasks = 1;
    int chunkSize = 200;
    int norm = 2;
    boolean sequentialAccessOutput = true;
    
    String inputDir = "inputDir";
   
    Configuration conf = new Configuration();
    FileSystem fs = FileSystem.get(conf);
    /*
     * SequenceFile.Writer writer = new SequenceFile.Writer(fs, conf, new Path(inputDir, "documents.seq"),
     * Text.class, Text.class); for (Document d : Database) { writer.append(new Text(d.getID()), new
     * Text(d.contents())); } writer.close();
     */

    String outputDir = "newsClusters";
    HadoopUtil.delete(conf, new Path(outputDir));
    Path tokenizedPath = new Path(outputDir,
        DocumentProcessor.TOKENIZED_DOCUMENT_OUTPUT_FOLDER);
    MyAnalyzer analyzer = new MyAnalyzer();
    DocumentProcessor.tokenizeDocuments(new Path(inputDir), analyzer.getClass()
        .asSubclass(Analyzer.class), tokenizedPath, conf);
    
    DictionaryVectorizer.createTermFrequencyVectors(tokenizedPath,
      new Path(outputDir), conf, minSupport, maxNGramSize, minLLRValue, 2, true, reduceTasks,
      chunkSize, sequentialAccessOutput, false);
    TFIDFConverter.processTfIdf(
      new Path(outputDir , DictionaryVectorizer.DOCUMENT_VECTOR_OUTPUT_FOLDER),
      new Path(outputDir), conf, chunkSize, minDf,
      maxDFPercent, norm, true, sequentialAccessOutput, false, reduceTasks);
    Path vectorsFolder = new Path(outputDir, "tfidf-vectors");
    Path canopyCentroids = new Path(outputDir , "canopy-centroids");
    Path clusterOutput = new Path(outputDir , "clusters");
    
    CanopyDriver.run(vectorsFolder, canopyCentroids,
      new EuclideanDistanceMeasure(), 250, 120, false, false);
    KMeansDriver.run(conf, vectorsFolder, new Path(canopyCentroids, "clusters-0"),
      clusterOutput, new TanimotoDistanceMeasure(), 0.01,
      20, true, false);
    
    SequenceFile.Reader reader = new SequenceFile.Reader(fs,
        new Path(clusterOutput + Cluster.CLUSTERED_POINTS_DIR + "/part-00000"), conf);
    
    IntWritable key = new IntWritable();
    WeightedVectorWritable value = new WeightedVectorWritable();
    while (reader.next(key, value)) {
       System.out.println(key.toString() + " belongs to cluster "
       + value.toString());
    }
    reader.close();
  }
}
