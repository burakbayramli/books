package mia.recommender.ch04;

import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.impl.recommender.ClusterSimilarity;
import org.apache.mahout.cf.taste.impl.recommender.FarthestNeighborClusterSimilarity;
import org.apache.mahout.cf.taste.impl.recommender.TreeClusteringRecommender;
import org.apache.mahout.cf.taste.impl.similarity.LogLikelihoodSimilarity;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.similarity.UserSimilarity;

class ClusterBasedRecommender {

  Recommender buildRecommender(DataModel model) throws TasteException {
    UserSimilarity similarity = new LogLikelihoodSimilarity(model);
    ClusterSimilarity clusterSimilarity =
        new FarthestNeighborClusterSimilarity(similarity);
    return new TreeClusteringRecommender(model, clusterSimilarity, 10);
  }

}