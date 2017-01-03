/*
 * Source code for Listing 9.7
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
import org.apache.mahout.clustering.fuzzykmeans.FuzzyKMeansDriver;
import org.apache.mahout.common.HadoopUtil;
import org.apache.mahout.common.distance.ManhattanDistanceMeasure;
import org.apache.mahout.common.distance.TanimotoDistanceMeasure;
import org.apache.mahout.vectorizer.DictionaryVectorizer;
import org.apache.mahout.vectorizer.DocumentProcessor;
import org.apache.mahout.vectorizer.tfidf.TFIDFConverter;

public class NewsFuzzyKMeansClustering {
  
  public static void main(String args[]) throws Exception {
    
    int minSupport = 5;
    int minDf = 10;
    int maxDFPercent = 70;
    int maxNGramSize = 1;
    int minLLRValue = 200;
    int reduceTasks = 1;
    int chunkSize = 200;
    int norm = 2;
    boolean sequentialAccessOutput = true;
    
    String inputDir = "inputDir";
    
    Configuration conf = new Configuration();
    FileSystem fs = FileSystem.get(conf);
    /*
    SequenceFile.Writer writer = new SequenceFile.Writer(fs, conf,
        new Path(inputDir, "documents.seq"), Text.class, Text.class);
    for (Document d : Database) {
      writer.append(new Text(d.getID()), new Text(d.contents()));
    }
    writer.close();*/
     
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
    String vectorsFolder = outputDir + "/tfidf-vectors";
    String canopyCentroids = outputDir + "/canopy-centroids";
    String clusterOutput = outputDir + "/clusters/";
    
    CanopyDriver.run(conf, new Path(vectorsFolder), new Path(canopyCentroids),
      new ManhattanDistanceMeasure(), 3000.0, 2000.0, false, false);
    
    FuzzyKMeansDriver.run(conf, new Path(vectorsFolder), new Path(canopyCentroids, "clusters-0"), new Path(clusterOutput),
      new TanimotoDistanceMeasure(), 0.01, 20, 2.0f, true, true, 0.0, false);
    
    SequenceFile.Reader reader = new SequenceFile.Reader(fs, new Path(
      clusterOutput + Cluster.CLUSTERED_POINTS_DIR +"/part-m-00000"), conf);
    
    IntWritable key = new IntWritable();
    WeightedVectorWritable value = new WeightedVectorWritable();
    while (reader.next(key, value)) {
      System.out.println("Cluster: " + key.toString() + " "
                         + value.getVector().asFormatString());
    }
    reader.close();
  }
}
