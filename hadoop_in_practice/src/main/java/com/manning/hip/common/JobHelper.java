package com.manning.hip.common;

import org.apache.commons.logging.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.util.StringUtils;

import java.io.*;
import java.util.*;

public class JobHelper {
  public static final Log
      log = LogFactory.getLog(JobHelper.class.getName());

  public static void addDirToCache(File dir, FileSystem fs,
                                   Set<String> localUrls) {
    if (null == dir) {
      return;
    }

    for (File libfile : dir.listFiles()) {
      if (libfile.exists() && !libfile.isDirectory()
          && libfile.getName().endsWith("jar")) {
        addToCache(libfile.toString(), fs, localUrls);
      }
    }
  }

  public static void addToCache(String file, FileSystem fs,
                                Set<String> localUrls) {
    if (null == file) {
      return;
    }

    Path p = new Path(file);
    String qualified = p.makeQualified(fs).toString();
    localUrls.add(qualified);
  }

  public static void addToCache(Configuration conf)
      throws IOException {
    String mavenDependencies = System.getProperty("MVN_CLASSPATH");

    for(String path: org.apache.commons.lang.StringUtils.split(mavenDependencies, ":")) {
      addJarForJob(conf, path);
    }
  }

  public static void addJarForJob(Configuration conf, String localFile)
      throws IOException {
    FileSystem fs = FileSystem.getLocal(conf);
    Set<String> localUrls = new HashSet<String>();

    addToCache(localFile, fs, localUrls);

    // If we didn't put anything in our set, then there's nothing to cache.
    if (localUrls.isEmpty()) {
      return;
    }

    // Add these to the 'tmpjars' array, which the MR JobSubmitter
    // will upload to HDFS and put in the DistributedCache libjars.
    String tmpjars = conf.get("tmpjars");
    StringBuilder sb = new StringBuilder();
    if (null != tmpjars) {
      sb.append(tmpjars);
      sb.append(",");
    }
    sb.append(
        StringUtils.arrayToString(localUrls.toArray(new String[0])));
    conf.set("tmpjars", sb.toString());
    //log.info("tmpjars = " + conf.get("tmpjars"));
  }

}
