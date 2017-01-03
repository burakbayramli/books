package com.manning.hip.ch3;

import org.apache.commons.logging.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hive.serde.Constants;
import org.apache.hadoop.hive.serde2.*;
import org.apache.hadoop.hive.serde2.objectinspector.*;
import org.apache.hadoop.hive.serde2.objectinspector.ObjectInspector.Category;
import org.apache.hadoop.hive.serde2.objectinspector.primitive.PrimitiveObjectInspectorFactory;
import org.apache.hadoop.hive.serde2.typeinfo.*;
import org.apache.hadoop.io.*;

import java.nio.charset.CharacterCodingException;
import java.util.*;
import java.util.regex.Pattern;


public class StockWritableSerDe implements SerDe {

  public static final Log LOG = LogFactory
      .getLog(StockWritableSerDe.class.getName());

  private List<String> columnNames;

  private boolean lastColumnTakesRest = false;
  private int splitLimit = -1;
  private StandardStructObjectInspector rowOI;
  private ArrayList<Object> row;
  private Object[] outputFields;

  @Override
  public String toString() {
    return "StockWritableSerDe[" + columnNames + "]";
  }

  public void checkType(List<String> columnNames, List<TypeInfo> columnTypes, int idx, TypeInfo expectedType)
      throws SerDeException {
    if (!columnTypes.get(idx).equals(expectedType)) {
      throw new SerDeException(getClass().getName()
          + " expected type " + expectedType.toString() + ", but column[" + idx + "] named "
          + columnNames.get(idx) + " has type " + columnTypes.get(idx));
    }
  }

  public void initialize(Configuration job, Properties tbl) throws SerDeException {
    String columnNameProperty = tbl.getProperty(Constants.LIST_COLUMNS);
    String columnTypeProperty = tbl.getProperty(Constants.LIST_COLUMN_TYPES);

    columnNames = Arrays.asList(columnNameProperty.split(","));
    List<TypeInfo> columnTypes = TypeInfoUtils
        .getTypeInfosFromTypeString(columnTypeProperty);

    assert columnNames.size() == columnTypes.size();
    assert columnNames.size() == 8;

    checkType(columnNames, columnTypes, 0, TypeInfoFactory.stringTypeInfo);
    checkType(columnNames, columnTypes, 1, TypeInfoFactory.stringTypeInfo);
    checkType(columnNames, columnTypes, 2, TypeInfoFactory.doubleTypeInfo);
    checkType(columnNames, columnTypes, 3, TypeInfoFactory.doubleTypeInfo);
    checkType(columnNames, columnTypes, 4, TypeInfoFactory.doubleTypeInfo);
    checkType(columnNames, columnTypes, 5, TypeInfoFactory.doubleTypeInfo);
    checkType(columnNames, columnTypes, 6, TypeInfoFactory.intTypeInfo);
    checkType(columnNames, columnTypes, 7, TypeInfoFactory.doubleTypeInfo);


    // Constructing the row ObjectInspector:
    // The row consists of some string columns, each column will be a java
    // String object.
    List<ObjectInspector> columnOIs = new ArrayList<ObjectInspector>(
        columnNames.size());
    columnOIs.add(PrimitiveObjectInspectorFactory.javaStringObjectInspector);
    columnOIs.add(PrimitiveObjectInspectorFactory.javaStringObjectInspector);
    columnOIs.add(PrimitiveObjectInspectorFactory.javaDoubleObjectInspector);
    columnOIs.add(PrimitiveObjectInspectorFactory.javaDoubleObjectInspector);
    columnOIs.add(PrimitiveObjectInspectorFactory.javaDoubleObjectInspector);
    columnOIs.add(PrimitiveObjectInspectorFactory.javaDoubleObjectInspector);
    columnOIs.add(PrimitiveObjectInspectorFactory.javaIntObjectInspector);
    columnOIs.add(PrimitiveObjectInspectorFactory.javaDoubleObjectInspector);

    // StandardStruct uses ArrayList to store the row.
    rowOI = ObjectInspectorFactory.getStandardStructObjectInspector(
        columnNames, columnOIs);

    // Constructing the row object, etc, which will be reused for all rows.
    row = new ArrayList<Object>(columnNames.size());
    for (String columnName : columnNames) {
      row.add(null);
    }
  }

  public Object deserialize(Writable field) throws SerDeException {
    if (field instanceof StockPriceWritable) {
      StockPriceWritable stock = (StockPriceWritable) field;
      row.set(0, stock.getSymbol());
      row.set(1, stock.getDate());
      row.set(2, stock.getOpen());
      row.set(3, stock.getHigh());
      row.set(4, stock.getLow());
      row.set(5, stock.getClose());
      row.set(6, stock.getVolume());
      row.set(7, stock.getAdjClose());
      return row;
    }
      throw new SerDeException(this.getClass().getName()
          + " unexpected Writable type " + field.getClass().getName());
  }

  public ObjectInspector getObjectInspector() throws SerDeException {
    return rowOI;
  }

  public Class<? extends Writable> getSerializedClass() {
    return NullWritable.class;
  }

  public Writable serialize(Object obj, ObjectInspector objInspector) throws SerDeException {
    return NullWritable.get();
  }

}
