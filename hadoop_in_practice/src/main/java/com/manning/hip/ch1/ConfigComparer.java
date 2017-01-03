package com.manning.hip.ch1;


import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.mapred.JobConf;

import java.lang.reflect.Field;
import java.util.*;

public final class ConfigComparer {
  private final Configuration defaultConfig;
  private final Configuration siteConfig;
  private Map<String, String> invalidSiteConfigEntries = new HashMap<String, String>();
  private List<ConfigProperty> configProperties;
  private final String defaultFile;
  private final String siteFile;

  public ConfigComparer(String defaultFile, String siteFile)
      throws NoSuchFieldException, IllegalAccessException {
    this.defaultFile = defaultFile;
    this.siteFile = siteFile;
    defaultConfig = loadConfiguration(defaultFile);
    siteConfig = loadConfiguration(siteFile);
    load();
  }

  private void load()
      throws NoSuchFieldException, IllegalAccessException {

    Map<String, String> siteConfMap = toMap(siteConfig);

    Set<String> finalParameters = getConfigFinalProperties(defaultConfig);

    configProperties = new ArrayList<ConfigProperty>();

    for(Map.Entry<String, String> entry: defaultConfig) {
      String key = entry.getKey();
      String defaultValue = entry.getValue();
      boolean mutable = !finalParameters.contains(key);
      String value = null;

      if(siteConfMap.containsKey(key)) {
        value = siteConfMap.get(key);
        siteConfMap.remove(key);
      }

      configProperties.add(new ConfigProperty(key, mutable, value, defaultValue));
    }

    Collections.sort(configProperties);

    invalidSiteConfigEntries = siteConfMap;
  }

  Map<String, String> toMap(Configuration c)
      throws NoSuchFieldException, IllegalAccessException {

    Map<String, String> props = new HashMap<String, String>();

    for(Map.Entry<String, String> entry: c) {
      props.put(entry.getKey(), entry.getValue());
    }
    return props;
  }

  public static Set<String> getConfigFinalProperties(Configuration c)
  throws NoSuchFieldException, IllegalAccessException {
    Field privateStringField = Configuration.class.
        getDeclaredField("finalParameters");

    privateStringField.setAccessible(true);

    return (Set<String>) privateStringField.get(c);
  }


  public static Configuration loadConfiguration(String file) {
    Configuration c = new Configuration(false);
    c.addResource(file);
    //c.reloadConfiguration();
    return c;
  }

  public static void main(String... args) throws Exception {

    Configuration conf = new Configuration();
    conf.writeXml(System.out);

    System.out.println("\n\n");

    JobConf jobConf = new JobConf(conf);

    jobConf.writeXml(System.out);
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).
       append("configProperties", configProperties).
       append("invalidSiteConfigEntries", invalidSiteConfigEntries).
       toString();
  }

  public String multiLineOutput() {
    StringBuilder sb = new StringBuilder();

    sb.append("Default file:  ").append(defaultFile);
    sb.append("\n");
    sb.append("Site file:  ").append(siteFile);
    sb.append("\n");

    sb.append(String.format("%42s %7s %42s %42s",
        "Name", "Final", "Default File Value", "Site File Value"));
    sb.append("\n");

    for(ConfigProperty c: configProperties) {
      sb.append(String.format("%42s %7b %42s %42s",
          StringUtils.abbreviate(c.getName(), 40),
          !c.isMutable(),
          StringUtils.abbreviate(c.getDefaultValue(), 40),
          StringUtils.abbreviate(c.getValue(), 40)));
      sb.append("\n");
    }

    for(Map.Entry<String, String> entry: invalidSiteConfigEntries.entrySet()) {
      sb.append(String.format("%42s %7s %42s %42s",
          StringUtils.abbreviate(entry.getKey(), 40),
          "-",
          "-",
          StringUtils.abbreviate(entry.getValue(), 40)));
      sb.append("\n");
    }
    return sb.toString();

  }
}
