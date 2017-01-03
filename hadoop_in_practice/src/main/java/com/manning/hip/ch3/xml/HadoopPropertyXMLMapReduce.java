package com.manning.hip.ch3.xml;


import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.*;
import org.slf4j.*;

import javax.xml.stream.*;
import java.io.*;

import static javax.xml.stream.XMLStreamConstants.*;

public final class HadoopPropertyXMLMapReduce {
  private static final Logger log = LoggerFactory.getLogger
      (HadoopPropertyXMLMapReduce.class);

  public static class Map extends Mapper<LongWritable, Text,
      Text, Text> {

    @Override
    protected void map(LongWritable key, Text value,
                       Mapper.Context context)
        throws
        IOException, InterruptedException {
      String document = value.toString();
      System.out.println("'" + document + "'");
      try {
        XMLStreamReader reader =
            XMLInputFactory.newInstance().createXMLStreamReader(new
                ByteArrayInputStream(document.getBytes()));
        String propertyName = "";
        String propertyValue = "";
        String currentElement = "";
        while (reader.hasNext()) {
          int code = reader.next();
          switch (code) {
            case START_ELEMENT:
              currentElement = reader.getLocalName();
              break;
            case CHARACTERS:
              if (currentElement.equalsIgnoreCase("name")) {
                propertyName += reader.getText();
              } else if (currentElement.equalsIgnoreCase("value")) {
                propertyValue += reader.getText();
              }
              break;
          }
        }
        reader.close();
        context.write(propertyName.trim(), propertyValue.trim());
      } catch (Exception e) {
        log.error("Error processing '" + document + "'", e);
      }
    }
  }

  public static void main(String... args) throws Exception {
    runJob(args[0], args[1]);
  }

  public static void runJob(String input,
                            String output)
      throws Exception {
    Configuration conf = new Configuration();
    conf.set("key.value.separator.in.input.line", " ");
    conf.set("xmlinput.start", "<property>");
    conf.set("xmlinput.end", "</property>");

    Job job = new Job(conf);
    job.setJarByClass(HadoopPropertyXMLMapReduce.class);
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(Text.class);
    job.setMapperClass(Map.class);
    job.setInputFormatClass(XmlInputFormat.class);
    job.setNumReduceTasks(0);
    job.setOutputFormatClass(TextOutputFormat.class);

    FileInputFormat.setInputPaths(job, new Path(input));
    Path outPath = new Path(output);
    FileOutputFormat.setOutputPath(job, outPath);
    outPath.getFileSystem(conf).delete(outPath, true);

    job.waitForCompletion(true);
  }
}
