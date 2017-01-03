package com.manning.hip.ch10;

import com.maxmind.geoip.LookupService;
import org.apache.hadoop.hive.ql.exec.Description;
import org.apache.hadoop.hive.ql.exec.UDFArgumentException;
import org.apache.hadoop.hive.ql.exec.UDFArgumentLengthException;
import org.apache.hadoop.hive.ql.metadata.HiveException;
import org.apache.hadoop.hive.ql.udf.generic.GenericUDF;
import org.apache.hadoop.hive.serde2.objectinspector.ObjectInspector;
import org.apache.hadoop.hive.serde2.objectinspector.ObjectInspectorConverters;
import org.apache.hadoop.hive.serde2.objectinspector.PrimitiveObjectInspector;
import org.apache.hadoop.hive.serde2.objectinspector.primitive.PrimitiveObjectInspectorFactory;
import org.apache.hadoop.io.Text;

import java.io.IOException;
import java.net.URL;

@Description(
    name = "country",
    value = "_FUNC_(ip, geolocfile) - Returns the geolocated country code " +
    " for the IP"
)
public class GeolocUDF extends GenericUDF {
  private LookupService geoloc;
  private ObjectInspectorConverters.Converter[] converters;

  @Override
  public ObjectInspector initialize(ObjectInspector[] arguments) throws UDFArgumentException {
    if (arguments.length != 2) {
      throw new UDFArgumentLengthException(
          "The function COUNTRY(ip, geolocfile) takes exactly 2 arguments.");
    }

    converters = new ObjectInspectorConverters.Converter[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      converters[i] = ObjectInspectorConverters.getConverter(arguments[i],
          PrimitiveObjectInspectorFactory.javaStringObjectInspector);
    }

    return PrimitiveObjectInspectorFactory
        .getPrimitiveJavaObjectInspector(PrimitiveObjectInspector.PrimitiveCategory.STRING);
  }

  @Override
  public Object evaluate(GenericUDF.DeferredObject[] arguments) throws HiveException {
    assert (arguments.length == 2);

    if (arguments[0].get() == null || arguments[1].get() == null) {
      return null;
    }

    String ip = (String) converters[0].convert(arguments[0].get());
    String filename = (String) converters[1].convert(arguments[1].get());

    return lookup(ip, filename);
  }

  protected String lookup(String ip, String filename) throws HiveException {
    try {
      if (geoloc == null) {
        URL u = getClass().getClassLoader().getResource(filename);
        if (u == null) {
          throw new HiveException("Couldn't find geolocation file '" + filename + "'");
        }
        geoloc =
            new LookupService(u.getFile(), LookupService.GEOIP_MEMORY_CACHE);
      }

      String countryCode = geoloc.getCountry(ip).getCode();

      if ("--".equals(countryCode)) {
        return null;
      }

      return countryCode;
    } catch (IOException e) {
      throw new HiveException("Caught IO exception", e);
    }
  }

  @Override
  public String getDisplayString(String[] children) {
    assert (children.length == 2);
    return "country(" + children[0] + ", " + children[1] + ")";
  }
}
