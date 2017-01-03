package com.manning.hip.ch3.csv;

import com.manning.hip.ch3.TextArrayWritable;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.io.compress.CompressionCodecFactory;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.JobContext;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.LineRecordReader;

import java.io.IOException;

/**
 * An {@link org.apache.hadoop.mapreduce.InputFormat} for CSV
 * plain text files.  Keys are byte offsets in
 * the file, and values are {@link org.apache.hadoop.io.ArrayWritable}'s with tokenized
 * values.
 */
public class CSVInputFormat extends
    FileInputFormat<LongWritable, TextArrayWritable> {

  public static String CSV_TOKEN_SEPARATOR_CONFIG =
      "csvinputformat.token.delimiter";

  @Override
  public RecordReader<LongWritable, TextArrayWritable>
  createRecordReader(InputSplit split,
                     TaskAttemptContext context) {
    String csvDelimiter = context.getConfiguration().get( //<co id="ch02_comment_csv_inputformat1"/>
        CSV_TOKEN_SEPARATOR_CONFIG);

    Character separator = null;
    if(csvDelimiter != null && csvDelimiter.length() == 1) {
      separator = csvDelimiter.charAt(0);
    }

    return new CSVRecordReader(separator);             //<co id="ch02_comment_csv_inputformat2"/>
  }

  @Override
  protected boolean isSplitable(JobContext context, Path file) {
    CompressionCodec codec =
        new CompressionCodecFactory(context.getConfiguration())
            .getCodec(file);
    return codec == null;    //<co id="ch02_comment_csv_inputformat3"/>
  }

  public static class CSVRecordReader              //<co id="ch02_comment_csv_inputformat4"/>
      extends RecordReader<LongWritable, TextArrayWritable> {
    private LineRecordReader reader;
    private TextArrayWritable value;
    private final CSVParser parser;

    public CSVRecordReader(Character csvDelimiter) {
      this.reader = new LineRecordReader();
      if (csvDelimiter == null) {
        parser = new CSVParser();             //<co id="ch02_comment_csv_inputformat5"/>
      } else {
        parser = new CSVParser(csvDelimiter);
      }
    }

    @Override
    public void initialize(InputSplit split,
                           TaskAttemptContext context)
        throws IOException, InterruptedException {
      reader.initialize(split, context);     //<co id="ch02_comment_csv_inputformat6"/>
    }

    @Override
    public boolean nextKeyValue()
        throws IOException, InterruptedException {
      if (reader.nextKeyValue()) {       //<co id="ch02_comment_csv_inputformat7"/>
        loadCSV();                        //<co id="ch02_comment_csv_inputformat8"/>
        return true;
      } else {
        value = null;
        return false;
      }
    }

    private void loadCSV() throws IOException {            //<co id="ch02_comment_csv_inputformat9"/>
      String line = reader.getCurrentValue().toString();
      String[] tokens = parser.parseLine(line);            //<co id="ch02_comment_csv_inputformat10"/>
      value = new TextArrayWritable(convert(tokens));
    }

    private Text[] convert(String[] s) {
      Text t[] = new Text[s.length];
      for(int i=0; i < t.length; i++) {
        t[i] = new Text(s[i]);
      }
      return t;
    }

    @Override
    public LongWritable getCurrentKey()      //<co id="ch02_comment_csv_inputformat11"/>
        throws IOException, InterruptedException {
      return reader.getCurrentKey();
    }

    @Override
    public TextArrayWritable getCurrentValue()    //<co id="ch02_comment_csv_inputformat12"/>
        throws IOException, InterruptedException {
      return value;
    }

    @Override
    public float getProgress()
        throws IOException, InterruptedException {
      return reader.getProgress();
    }

    @Override
    public void close() throws IOException {
      reader.close();
    }
  }
}
