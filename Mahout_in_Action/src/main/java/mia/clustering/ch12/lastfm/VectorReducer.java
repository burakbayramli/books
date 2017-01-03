package mia.clustering.ch12.lastfm;

import java.io.IOException;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.mahout.math.NamedVector;
import org.apache.mahout.math.Vector;
import org.apache.mahout.math.VectorWritable;

public class VectorReducer extends
    Reducer<Text,VectorWritable,Text,VectorWritable> {
  private VectorWritable writer = new VectorWritable();
  
  @Override
  protected void reduce(Text tag,
                        Iterable<VectorWritable> values,
                        Context context) throws IOException,
                                        InterruptedException {
    Vector vector = null;
    for (VectorWritable partialVector : values) {
      if (vector == null) {
        vector = partialVector.get().like();
      }
      partialVector.get().addTo(vector);
    }
    NamedVector namedVector = new NamedVector(vector, tag.toString());
    writer.set(namedVector);
    context.write(tag, writer);
  }
}
