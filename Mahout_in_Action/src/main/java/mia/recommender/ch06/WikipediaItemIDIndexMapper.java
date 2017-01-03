package mia.recommender.ch06;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.mahout.math.VarIntWritable;
import org.apache.mahout.math.VarLongWritable;

public final class WikipediaItemIDIndexMapper extends
    Mapper<LongWritable,Text,VarIntWritable, VarLongWritable> {

  private static final Pattern NUMBERS = Pattern.compile("(\\d+)");

  @Override
  protected void map(LongWritable key,
                     Text value,
                     Context context) throws IOException, InterruptedException {
    String line = value.toString();
    Matcher m = NUMBERS.matcher(line);
    m.find();
    VarIntWritable index = new VarIntWritable();
    VarLongWritable itemID = new VarLongWritable();
    while (m.find()) {
      long item = Long.parseLong(m.group());
      itemID.set(item);
      index.set(idToIndex(item));
      context.write(index, itemID);
    }
  }

  static int idToIndex(long itemID) {
    return 0x7FFFFFFF & ((int) itemID ^ (int) (itemID >>> 32));
  }

}