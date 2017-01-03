package mia.clustering.ch09;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.SequenceFile;
import org.apache.hadoop.io.Text;
import org.apache.lucene.analysis.Analyzer;
import org.apache.mahout.common.HadoopUtil;
import org.apache.mahout.math.VectorWritable;
import org.apache.mahout.vectorizer.DictionaryVectorizer;
import org.apache.mahout.vectorizer.DocumentProcessor;
import org.apache.mahout.vectorizer.tfidf.TFIDFConverter;

public class ReutersToSparseVectors {
  
  public static void main(String args[]) throws Exception {
    
    int minSupport = 5;
    int minDf = 5;
    int maxDFPercent = 95;
    int maxNGramSize = 1;
    float minLLRValue = 50;
    int reduceTasks = 1;
    int chunkSize = 200;
    int norm = 2;
    boolean sequentialAccessOutput = true;
    
    String inputDir = "inputDir";

    Configuration conf = new Configuration();
    FileSystem fs = FileSystem.get(conf);

    String outputDir = "reuters";
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
    
    SequenceFile.Reader reader = new SequenceFile.Reader(fs,
        new Path(vectorsFolder, "part-r-00000"), conf);
 
    Text key = new Text();
    VectorWritable value = new VectorWritable();
    while (reader.next(key, value)) {
      System.out.println(key.toString() + " = > "
                         + value.get().asFormatString());
    }
    reader.close();
  }
}
