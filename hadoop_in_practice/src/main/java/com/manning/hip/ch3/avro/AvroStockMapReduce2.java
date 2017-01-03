package com.manning.hip.ch3.avro;

import com.manning.hip.ch5.SmallFilesMapReduce;
import com.manning.hip.ch3.avro.gen.Stock;
import org.apache.avro.Schema;
import org.apache.avro.mapred.*;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.mapred.*;

import java.io.IOException;

import static org.apache.avro.file.DataFileConstants.SNAPPY_CODEC;

public class AvroStockMapReduce2 {

  public static void main(String... args) throws Exception {
    JobConf job = new JobConf();
    job.setJarByClass(SmallFilesMapReduce.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    AvroJob.setInputSchema(job,
        Stock.SCHEMA$);  //<co id="ch03_avro_mr_comment1"/>
    AvroJob.setMapOutputSchema(job, Pair.getPairSchema(Stock.SCHEMA$,
        Schema.create(Schema.Type.NULL)));
    AvroJob.setOutputSchema(job,
        Stock.SCHEMA$);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    AvroJob.setMapperClass(job,
        Mapper.class);    //<co id="ch03_smallfilemr_comment2"/>
    AvroJob.setReducerClass(job,
        Reducer.class);

    FileOutputFormat.setCompressOutput(job, true);
    AvroJob.setOutputCodec(job, SNAPPY_CODEC);

    JobClient.runJob(job);
  }

  public static class Mapper
      extends
      AvroMapper<Stock, Pair<Stock, Void>> {   //<co id="ch03_smallfilemr_comment3"/>

    @Override
    public void map(Stock stock,
                    AvroCollector<Pair<Stock, Void>> collector,
                    Reporter reporter) throws IOException {
      collector.collect(new Pair<Stock, Void>(stock, (Void) null));
    }
  }

  public static class Reducer
      extends AvroReducer<Stock, Void, Stock> {
    @Override
    public void reduce(Stock w, Iterable<Void> ignore,
                       AvroCollector<Stock> collector,
                       Reporter reporter) throws IOException {
      collector.collect(w);
    }
  }
}
