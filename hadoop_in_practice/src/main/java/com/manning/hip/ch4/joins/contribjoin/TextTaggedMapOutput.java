package com.manning.hip.ch4.joins.contribjoin;

import org.apache.hadoop.contrib.utils.join.TaggedMapOutput;
import org.apache.hadoop.io.*;

import java.io.*;

public class TextTaggedMapOutput extends TaggedMapOutput {

  private Text data;

  public TextTaggedMapOutput() {
    this.data = new Text("");
  }

  public TextTaggedMapOutput(Text data) {
    this.data = data;
  }

  public Writable getData() {
    return data;
  }

  public void setData(Text data) {
    this.data = data;
  }

  public void write(DataOutput out) throws IOException {
    this.tag.write(out);
    this.data.write(out);
  }

  public void readFields(DataInput in) throws IOException {
    this.tag.readFields(in);
    this.data.readFields(in);
  }
}
