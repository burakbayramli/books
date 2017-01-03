package com.manning.hip.ch1;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

public class ConfigProperty implements Comparable<ConfigProperty> {
  private final String name;
  private final boolean mutable;
  private final String value;
  private final String defaultValue;

  public ConfigProperty(String name,
                        boolean mutable,
                        String value,
                        String defaultValue) {
    this.name = name;
    this.mutable = mutable;
    this.value = value;
    this.defaultValue = defaultValue;
  }

  public String getName() {
    return name;
  }

  public boolean isMutable() {
    return mutable;
  }

  public String getValue() {
    return value;
  }

  public String getDefaultValue() {
    return defaultValue;
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (obj == this) {
      return true;
    }
    if (obj.getClass() != getClass()) {
      return false;
    }
    ConfigProperty rhs = (ConfigProperty) obj;
    return new EqualsBuilder().append(getName(), rhs.getName())
        .isEquals();
  }

  @Override
  public String toString() {
    return ReflectionToStringBuilder.toString(this);
  }

  @Override
  public int compareTo(ConfigProperty configProperty) {
    return getName().compareTo(configProperty.getName());
  }
}
