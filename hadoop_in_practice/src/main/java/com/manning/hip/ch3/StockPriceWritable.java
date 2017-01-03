package com.manning.hip.ch3;

import com.manning.hip.ch3.csv.CSVParser;
import org.apache.commons.lang.builder.*;
import org.apache.hadoop.io.*;

import java.io.*;

public class StockPriceWritable
    implements WritableComparable<StockPriceWritable>, Cloneable {
  String symbol;
  String date;
  double open;
  double high;
  double low;
  double close;
  int volume;
  double adjClose;

  public StockPriceWritable() {
  }

  public StockPriceWritable(String symbol,
                            String date,
                            double open,
                            double high,
                            double low,
                            double close,
                            int volume,
                            double adjClose) {
    this.symbol = symbol;
    this.date = date;
    this.open = open;
    this.high = high;
    this.low = low;
    this.close = close;
    this.volume = volume;
    this.adjClose = adjClose;
  }

  @Override
  public void write(DataOutput out) throws IOException {
    WritableUtils.writeString(out, symbol);
    WritableUtils.writeString(out, date);
    out.writeDouble(open);
    out.writeDouble(high);
    out.writeDouble(low);
    out.writeDouble(close);
    out.writeInt(volume);
    out.writeDouble(adjClose);
  }

  @Override
  public void readFields(DataInput in) throws IOException {
    symbol = WritableUtils.readString(in);
    date = WritableUtils.readString(in);
    open = in.readDouble();
    high = in.readDouble();
    low = in.readDouble();
    close = in.readDouble();
    volume = in.readInt();
    adjClose = in.readDouble();
  }

  @Override
  public int compareTo(StockPriceWritable passwd) {
    return CompareToBuilder.reflectionCompare(this, passwd);
  }

  @Override
  public String toString() {
    return ToStringBuilder
        .reflectionToString(this, ToStringStyle.SHORT_PREFIX_STYLE);
  }

  public String getSymbol() {
    return symbol;
  }

  public void setSymbol(String symbol) {
    this.symbol = symbol;
  }

  public String getDate() {
    return date;
  }

  public void setDate(String date) {
    this.date = date;
  }

  public double getOpen() {
    return open;
  }

  public void setOpen(double open) {
    this.open = open;
  }

  public double getHigh() {
    return high;
  }

  public void setHigh(double high) {
    this.high = high;
  }

  public double getLow() {
    return low;
  }

  public void setLow(double low) {
    this.low = low;
  }

  public double getClose() {
    return close;
  }

  public void setClose(double close) {
    this.close = close;
  }

  public int getVolume() {
    return volume;
  }

  public void setVolume(int volume) {
    this.volume = volume;
  }

  public double getAdjClose() {
    return adjClose;
  }

  public void setAdjClose(double adjClose) {
    this.adjClose = adjClose;
  }

  public static StockPriceWritable fromLine(String line)
      throws IOException {
    CSVParser parser = new CSVParser();
    String[] parts = parser.parseLine(line);

    StockPriceWritable stock = new StockPriceWritable(
        //<co id="ch03_comment_seqfile_write3"/>
        parts[0], parts[1], Double.valueOf(parts[2]),
        Double.valueOf(parts[3]),
        Double.valueOf(parts[4]),
        Double.valueOf(parts[5]),
        Integer.valueOf(parts[6]),
        Double.valueOf(parts[7])
    );
    return stock;
  }
}
