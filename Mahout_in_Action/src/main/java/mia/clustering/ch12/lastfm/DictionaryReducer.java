package mia.clustering.ch12.lastfm;

import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;

public class DictionaryReducer extends Reducer<Text,IntWritable,Text,IntWritable> { 
  @Override
  protected void reduce(Text artist,
                        Iterable<IntWritable> values,
                        Context context) throws IOException,
                                        InterruptedException {
    context.write(artist, new IntWritable(0));
  }
}
