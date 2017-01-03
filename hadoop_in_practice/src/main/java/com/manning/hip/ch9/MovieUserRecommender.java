package com.manning.hip.ch9;

import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.impl.neighborhood.NearestNUserNeighborhood;
import org.apache.mahout.cf.taste.impl.recommender.CachingRecommender;
import org.apache.mahout.cf.taste.impl.recommender.GenericUserBasedRecommender;
import org.apache.mahout.cf.taste.impl.similarity.PearsonCorrelationSimilarity;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.neighborhood.UserNeighborhood;
import org.apache.mahout.cf.taste.recommender.RecommendedItem;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.similarity.UserSimilarity;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class MovieUserRecommender {

  public static void main(String ... args) {
    try {
      recommend(args[0], 1, 2, 3);
    } catch (Throwable e) {
      e.printStackTrace();
    }
  }

  private static void recommend(String ratingsFile, int ... userIds)
      throws TasteException, IOException {
    DataModel model = new FileDataModel(new File(ratingsFile));

    UserSimilarity similarity = new PearsonCorrelationSimilarity(model);

    UserNeighborhood neighborhood =
        new NearestNUserNeighborhood(
            100, similarity, model);

    Recommender recommender =  new GenericUserBasedRecommender(
        model, neighborhood, similarity);

    Recommender cachingRecommender = new CachingRecommender(recommender);

    for(int userId: userIds) {
      System.out.println("UserID " + userId);
      List<RecommendedItem> recommendations =
          cachingRecommender.recommend(userId, 2);
      for(RecommendedItem item: recommendations) {
        System.out.println("  item " + item.getItemID() + " score " + item.getValue());
      }
    }
  }
}
