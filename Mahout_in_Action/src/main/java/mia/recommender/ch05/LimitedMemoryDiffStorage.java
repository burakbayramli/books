/*
 * Source code for listing 5.1
 * 
 */
package mia.recommender.ch05;

import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.common.Weighting;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.MemoryDiffStorage;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.SlopeOneRecommender;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.recommender.slopeone.DiffStorage;

class LimitedMemoryDiffStorage {

  Recommender buildRecommender(DataModel model) throws TasteException {
    DiffStorage diffStorage = new MemoryDiffStorage(
        model, Weighting.WEIGHTED, 10000000L);
    return new SlopeOneRecommender(
        model, Weighting.WEIGHTED, Weighting.WEIGHTED, diffStorage);
  }

}
