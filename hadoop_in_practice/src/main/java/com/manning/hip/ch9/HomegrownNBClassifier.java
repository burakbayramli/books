package com.manning.hip.ch9;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.mutable.MutableInt;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class HomegrownNBClassifier {

  public static void main(String... args) {

    Classifier c = new Classifier();

    c.train("ham windyhill roofing estimate");
    c.train("ham quick hadoop meetup");
    c.train("spam cheap quick xanax");
    c.train("spam quick easy money");

    System.out.println(c);

    c.classify("quick money");
  }


  public static class Classifier {
    Map<String, Category> categories = new HashMap<String, Category>();
    int numDocuments;

    public void train(String document) {
      numDocuments++;
      String[] parts = StringUtils.split(document);
      String category = parts[0];
      List<String> words = Arrays.asList(
          Arrays.copyOfRange(parts, 1, parts.length));
      Category cat = categories.get(category);
      if (cat == null) {
        cat = new Category(category);
        categories.put(category, cat);
      }
      cat.train(words);
      for (Category c : categories.values()) {
        c.updateProbability(numDocuments);
      }
    }

    public void classify(String words) {
      String[] parts = StringUtils.split(words);

      for (Category c : categories.values()) {
        double p = 1.0;
        for (String word : parts) {
          p *= c.weightedProbability(word);
        }
        System.out.println("Probability of document '" + words +
            "' for category '" + c.label +
            "' is " + (p * c.categoryProbability));
      }
    }

    public String toString() {
      StringBuilder sb = new StringBuilder();
      for (Category cat : categories.values()) {
        sb.append(cat).append("\n");
      }
      return sb.toString();
    }
  }

  public static class Category {
    String label;
    int numDocuments;
    double categoryProbability;
    Map<String, MutableInt> features =
        new HashMap<String, MutableInt>();

    public Category(String label) {
      this.label = label;
    }

    void train(List<String> words) {
      numDocuments++;
      for (String word : words) {
        MutableInt i = features.get(word);
        if (i == null) {
          i = new MutableInt(0);
          features.put(word, i);
        }
        i.increment();
      }
    }

    void updateProbability(int totalDocuments) {
      categoryProbability = (double) numDocuments /
          (double) totalDocuments;
    }

    double weightedProbability(String word) {
      MutableInt i = features.get(word);
      return (i == null ? 0.1 : (i.doubleValue() /
          (double) numDocuments));
    }

    public String toString() {
      StringBuilder sb = new StringBuilder();
      sb.append("Category = ").append(label)
          .append(", numDocs = ").append(numDocuments)
          .append(", categoryProbability = ")
          .append(categoryProbability);
      for (Map.Entry<String, MutableInt> entry : features.entrySet()) {
        sb.append("\n  ").append(entry.getKey()).append(" ")
            .append(entry.getValue());
      }
      return sb.toString();
    }
  }
}
