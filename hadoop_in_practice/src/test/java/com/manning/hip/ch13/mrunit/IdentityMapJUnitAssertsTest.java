package com.manning.hip.ch13.mrunit;

import junit.framework.TestCase;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mrunit.mapreduce.MapDriver;
import org.apache.hadoop.mrunit.types.Pair;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.List;

/**
 * Example test of the IdentityMapper to demonstrate proper MapDriver
 * usage in a test case.
 */
public class IdentityMapJUnitAssertsTest extends TestCase {

  private Mapper<Text, Text, Text, Text> mapper;
  private MapDriver<Text, Text, Text, Text> driver;

  @Before
  public void setUp() {
    mapper = new Mapper<Text, Text, Text, Text>();
    driver = new MapDriver<Text, Text, Text, Text>(mapper);
  }

  @Test
  public void testIdentityMapper() throws IOException {
    List<Pair<Text, Text>> results = driver
        .withInput(new Text("foo"), new Text("bar"))
        .withOutput(new Text("foo"), new Text("bar"))
        .run();

    MRUnitJUnitAsserts.assertOutputs(driver, results);
  }

}

