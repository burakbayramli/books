package mia.recommender.ch04;

import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.common.Weighting;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.MemoryDiffStorage;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.SlopeOneRecommender;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.recommender.slopeone.DiffStorage;

class SlopeOneNoWeighting {

  Recommender buildRecommender(DataModel model) throws TasteException {
    DiffStorage diffStorage = new MemoryDiffStorage(
        model, Weighting.UNWEIGHTED, Long.MAX_VALUE);
    return new SlopeOneRecommender(
      model,
      Weighting.UNWEIGHTED,
      Weighting.UNWEIGHTED,
      diffStorage);
  }

}