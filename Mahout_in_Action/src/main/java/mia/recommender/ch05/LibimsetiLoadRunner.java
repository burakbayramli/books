package mia.recommender.ch05;

import org.apache.mahout.cf.taste.impl.eval.LoadEvaluator;
import org.apache.mahout.cf.taste.impl.model.file.FileDataModel;
import org.apache.mahout.cf.taste.model.DataModel;
import org.apache.mahout.cf.taste.recommender.Recommender;

import java.io.File;

class LibimsetiLoadRunner {

  private LibimsetiLoadRunner() {
  }

  public static void main(String[] args) throws Exception {
    DataModel model = new FileDataModel(new File("ratings.dat"));
    Recommender rec = new LibimsetiRecommender(model);
    LoadEvaluator.runLoad(rec);
  }

}