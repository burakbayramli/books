package com.manning.hip.ch3.avro;

import com.manning.hip.ch3.csv.CSVParser;
import com.manning.hip.ch3.avro.gen.Stock;
import org.apache.avro.file.CodecFactory;
import org.apache.avro.file.DataFileWriter;
import org.apache.avro.specific.SpecificDatumWriter;
import org.apache.commons.io.FileUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

public class AvroStockFileWrite {

  static CSVParser parser = new CSVParser();

  public static Stock createStock(String line) throws IOException {

    String parts[] = parser.parseLine(line);
    Stock stock = new Stock();

    stock.setSymbol(parts[0]);
    stock.setDate(parts[1]);
    stock.setOpen(Double.valueOf(parts[2]));
    stock.setHigh(Double.valueOf(parts[3]));
    stock.setLow(Double.valueOf(parts[4]));
    stock.setClose(Double.valueOf(parts[5]));
    stock.setVolume(Integer.valueOf(parts[6]));
    stock.setAdjClose(Double.valueOf(parts[7]));

    return stock;
  }

  public static void writeToAvro(File inputFile, OutputStream outputStream)
      throws IOException {

    DataFileWriter<Stock> writer = //<co id="ch03_avrospecific_comment1"/>
        new DataFileWriter<Stock>(
            new SpecificDatumWriter<Stock>())
        .setSyncInterval(100);       //<co id="ch03_avrospecific_comment2"/>

    writer.setCodec(CodecFactory.snappyCodec());   //<co id="ch03_avrospecific_comment3"/>
    writer.create(Stock.SCHEMA$, outputStream);    //<co id="ch03_avrospecific_comment4"/>

    for(String line: FileUtils.readLines(inputFile)) {
      writer.append(createStock(line));     //<co id="ch03_avrospecific_comment5"/>
    }

    IOUtils.closeStream(writer);
    IOUtils.closeStream(outputStream);
  }

  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    File inputFile = new File(args[0]);
    Path destFile = new Path(args[1]);

    OutputStream os = hdfs.create(destFile);
    writeToAvro(inputFile, os);
  }
}
