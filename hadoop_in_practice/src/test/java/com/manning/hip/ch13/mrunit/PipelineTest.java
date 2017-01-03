package com.manning.hip.ch13.mrunit;

import junit.framework.TestCase;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.Mapper;
import org.apache.hadoop.mapred.Reducer;
import org.apache.hadoop.mapred.lib.IdentityMapper;
import org.apache.hadoop.mapred.lib.IdentityReducer;
import org.apache.hadoop.mrunit.PipelineMapReduceDriver;
import org.apache.hadoop.mrunit.types.Pair;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.List;

/**
 * Example test of the IdentityMapper to demonstrate proper MapDriver
 * usage in a test case.
 */
public class PipelineTest extends TestCase {

  private Mapper<Text, Text, Text, Text> mapper1;
  private Reducer<Text, Text, Text, Text> reducer1;
  private Mapper<Text, Text, Text, Text> mapper2;
  private Reducer<Text, Text, Text, Text> reducer2;

  private PipelineMapReduceDriver<Text, Text, Text, Text> driver;

  @Before
  public void setUp() {
    mapper1 = new IdentityMapper<Text, Text>();
    reducer1 = new IdentityReducer<Text, Text>();
    mapper2 = new IdentityMapper<Text, Text>();
    reducer2 = new IdentityReducer<Text, Text>();
    driver = new PipelineMapReduceDriver<Text, Text, Text, Text>();
    driver.addMapReduce(new Pair<Mapper, Reducer>(mapper1, reducer1));
    driver.addMapReduce(new Pair<Mapper, Reducer>(mapper2, reducer2));
  }

  @Test
  public void testIdentityMapper() throws IOException {
    List<Pair<Text, Text>> results = driver
        .withInput(new Text("foo"), new Text("bar"))
        .withInput(new Text("foo2"), new Text("bar2"))
        .withOutput(new Text("foo"), new Text("bar"))
        .withOutput(new Text("foo2"), new Text("bar2"))
        .run();

    MRUnitJUnitAsserts.assertOutputs(driver, results);
  }
}

