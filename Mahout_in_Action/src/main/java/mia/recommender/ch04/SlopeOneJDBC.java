package mia.recommender.ch04;

import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.common.Weighting;
import org.apache.mahout.cf.taste.impl.model.jdbc.AbstractJDBCDataModel;
import org.apache.mahout.cf.taste.impl.model.jdbc.MySQLJDBCDataModel;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.SlopeOneRecommender;
import org.apache.mahout.cf.taste.impl.recommender.slopeone.jdbc.MySQLJDBCDiffStorage;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.recommender.slopeone.DiffStorage;

class SlopeOneJDBC {

  Recommender buildRecommender() throws TasteException {
    AbstractJDBCDataModel model = new MySQLJDBCDataModel();
    DiffStorage diffStorage = new MySQLJDBCDiffStorage(model);
    return new SlopeOneRecommender(
        model, Weighting.WEIGHTED, Weighting.WEIGHTED, diffStorage);
  }

}
