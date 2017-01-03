package com.manning.hip.ch9;

import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.eval.RecommenderBuilder;
import org.apache.mahout.cf.taste.eval.RecommenderEvaluator;
import org.apache.mahout.cf.taste.impl.eval.AverageAbsoluteDifferenceRecommenderEvaluator;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.impl.neighborhood.NearestNUserNeighborhood;
import org.apache.mahout.cf.taste.impl.recommender.GenericUserBasedRecommender;
import org.apache.mahout.cf.taste.impl.similarity.PearsonCorrelationSimilarity;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.neighborhood.UserNeighborhood;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.similarity.UserSimilarity;

import java.io.File;
import java.io.IOException;

public class MovieUserEvaluator {

  public static void main(String ... args) {
    try {
      evaluate(args[0]);
    } catch (Throwable e) {
      e.printStackTrace();
    }
  }

  public static void evaluate(String ratingsFile)
      throws TasteException, IOException {
    DataModel model = new FileDataModel(new File(ratingsFile));
    RecommenderEvaluator evaluator =
        new AverageAbsoluteDifferenceRecommenderEvaluator();
    RecommenderBuilder recommenderBuilder = new MyRecommendBuilder();
    evaluator.evaluate(
        recommenderBuilder,
        null,
        model,
        0.95,
        0.05
    );
  }

  public static class MyRecommendBuilder implements RecommenderBuilder {
    @Override
    public Recommender buildRecommender(DataModel model)
        throws TasteException {
      UserSimilarity similarity =
          new PearsonCorrelationSimilarity(model);

      UserNeighborhood neighborhood =
          new NearestNUserNeighborhood(
              100,
              similarity, model);

      return new GenericUserBasedRecommender(
          model, neighborhood, similarity);
    }
  }
}
