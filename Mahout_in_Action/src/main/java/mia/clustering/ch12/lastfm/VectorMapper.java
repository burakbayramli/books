package mia.clustering.ch12.lastfm;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.DefaultStringifier;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.util.GenericsUtil;
import org.apache.mahout.math.NamedVector;
import org.apache.mahout.math.SequentialAccessSparseVector;
import org.apache.mahout.math.VectorWritable;

public class VectorMapper extends
    Mapper<LongWritable,Text,Text,VectorWritable> {
  private Pattern splitter;
  private VectorWritable writer;
  private Map<String,Integer> dictionary = new HashMap<String,Integer>();
  
  @Override
  protected void map(LongWritable key, Text value, Context context) throws IOException,
                                                                   InterruptedException {
    String[] fields = splitter.split(value.toString());
    if (fields.length < 4) {
      context.getCounter("Map", "LinesWithErrors").increment(1);
      return;
    }
    String artist = fields[1];
    String tag = fields[2];
    double weight = Double.parseDouble(fields[3]);
    NamedVector vector = new NamedVector(
        new SequentialAccessSparseVector(dictionary.size()), tag);
    vector.set(dictionary.get(artist), weight);
    writer.set(vector);
    context.write(new Text(tag), writer);
  }
  
  @Override
  protected void setup(Context context) throws IOException,
                                       InterruptedException {
    super.setup(context);
    Configuration conf = context.getConfiguration();
    DefaultStringifier<Map<String,Integer>> mapStringifier 
        = new DefaultStringifier<Map<String,Integer>>(
              conf, GenericsUtil.getClass(dictionary));
    dictionary = mapStringifier.fromString(conf.get("dictionary"));
    
    splitter = Pattern.compile("<sep>");
    writer = new VectorWritable();
  }
}
