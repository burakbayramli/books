package mia.clustering.ch12.twitter;

import java.io.IOException;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;

public class ByKeyReducer extends Reducer<Text,Text,Text,Text> {
  
  @Override
  protected void reduce(Text key,
                        Iterable<Text> values,
                        Context context) throws IOException,
                                        InterruptedException {
    StringBuilder output = new StringBuilder();
    for (Text value : values) {
      output.append(value.toString()).append(" ");
    }
    context.write(key, new Text(output.toString().trim()));
  }
}
