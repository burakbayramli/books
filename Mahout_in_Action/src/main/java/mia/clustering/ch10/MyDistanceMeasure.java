/*
 * Source Code for Listing 10.6
 * 
 */
package mia.clustering.ch10;

import java.util.Collection;
import java.util.Collections;

import org.apache.hadoop.conf.Configuration;
import org.apache.mahout.common.distance.DistanceMeasure;
import org.apache.mahout.common.parameters.Parameter;
import org.apache.mahout.math.CardinalityException;
import org.apache.mahout.math.Vector;

public class MyDistanceMeasure implements DistanceMeasure {
  
  @Override
  public double distance(Vector v1, Vector v2) {
    if (v1.size() != v2.size()) {
      throw new CardinalityException(v1.size(), v2.size());
    }
    double lengthSquaredv1 = v1.getLengthSquared();
    double lengthSquaredv2 = v2.getLengthSquared();
    
    double dotProduct = v2.dot(v1);
    
    double denominator = Math.sqrt(lengthSquaredv1)
                         * Math.sqrt(lengthSquaredv2);
    
    // correct for floating-point rounding errors
    if (denominator < dotProduct) {
      denominator = dotProduct;
    }
    
    double distance = 1.0 - dotProduct / denominator;
    
    if (distance < 0.5) {
      return (1 - distance) * (distance * distance) + distance
             * Math.sqrt(distance);
    } else return Math.sqrt(distance);
  }
  
  @Override
  public double distance(double centroidLengthSquare,
                         Vector centroid,
                         Vector v) {
    return distance(centroid, v);
  }
    
  @Override
  public Collection<Parameter<?>> getParameters() {
    return Collections.emptyList();
  }

  @Override
  public void createParameters(String prefix, Configuration jobConf) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void configure(Configuration config) {
    // TODO Auto-generated method stub
    
  }
  
}
