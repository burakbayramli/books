package mia.recommender.ch03;

import org.apache.mahout.cf.taste.common.TasteException;
import org.apache.mahout.cf.taste.eval.DataModelBuilder;
import org.apache.mahout.cf.taste.eval.IRStatistics;
import org.apache.mahout.cf.taste.eval.RecommenderBuilder;
import org.apache.mahout.cf.taste.eval.RecommenderIRStatsEvaluator;
import org.apache.mahout.cf.taste.impl.common.FastByIDMap;
import org.apache.mahout.cf.taste.impl.eval.GenericRecommenderIRStatsEvaluator;
import org.apache.mahout.cf.taste.impl.model.GenericBooleanPrefDataModel;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.impl.neighborhood.NearestNUserNeighborhood;
import org.apache.mahout.cf.taste.impl.recommender.GenericBooleanPrefUserBasedRecommender;
import org.apache.mahout.cf.taste.impl.similarity.LogLikelihoodSimilarity;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.model.PreferenceArray;
import org.apache.mahout.cf.taste.neighborhood.UserNeighborhood;
import org.apache.mahout.cf.taste.recommender.Recommender;
import org.apache.mahout.cf.taste.similarity.UserSimilarity;

import java.io.File;

class IREvaluatorBooleanPrefIntro2 {

  private IREvaluatorBooleanPrefIntro2() {
  }

  public static void main(String[] args) throws Exception {
    DataModel model = new GenericBooleanPrefDataModel(
        GenericBooleanPrefDataModel.toDataMap(
          new FileDataModel(new File("ua.base"))));

    RecommenderIRStatsEvaluator evaluator =
      new GenericRecommenderIRStatsEvaluator();
    RecommenderBuilder recommenderBuilder = new RecommenderBuilder() {
      @Override
      public Recommender buildRecommender(DataModel model) throws TasteException {
        UserSimilarity similarity = new LogLikelihoodSimilarity(model);
        UserNeighborhood neighborhood =
          new NearestNUserNeighborhood(10, similarity, model);
        return new GenericBooleanPrefUserBasedRecommender(model, neighborhood, similarity);
      }
    };
    DataModelBuilder modelBuilder = new DataModelBuilder() {
      @Override
      public DataModel buildDataModel(FastByIDMap<PreferenceArray> trainingData) {
        return new GenericBooleanPrefDataModel(
          GenericBooleanPrefDataModel.toDataMap(trainingData));
      }
    };
    IRStatistics stats = evaluator.evaluate(
        recommenderBuilder, modelBuilder, model, null, 10,
        GenericRecommenderIRStatsEvaluator.CHOOSE_THRESHOLD,
        1.0);
    System.out.println(stats.getPrecision());
    System.out.println(stats.getRecall());
  }
}