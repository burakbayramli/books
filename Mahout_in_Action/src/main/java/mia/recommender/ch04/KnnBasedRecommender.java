package mia.recommender.ch04;

import org.apache.mahout.cf.taste.impl.recommender.knn.ConjugateGradientOptimizer;
import org.apache.mahout.cf.taste.impl.recommender.knn.KnnItemBasedRecommender;
import org.apache.mahout.cf.taste.impl.recommender.knn.Optimizer;
import org.apache.mahout.cf.taste.impl.similarity.LogLikelihoodSimilarity;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.similarity.ItemSimilarity;

class KnnBasedRecommender {

  Recommender buildRecommender(DataModel model) {
    ItemSimilarity similarity = new LogLikelihoodSimilarity(model);
    Optimizer optimizer = new ConjugateGradientOptimizer();
    return new KnnItemBasedRecommender(model, similarity, optimizer, 10);
  }

}
