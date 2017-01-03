/*
 * Copyright 2010 Ted Dunning. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 *
 *    1. Redistributions of source code must retain the above copyright notice, this list of
 *       conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright notice, this list
 *       of conditions and the following disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY <COPYRIGHT HOLDER> ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * The views and conclusions contained in the software and documentation are those of the
 * authors and should not be interpreted as representing official policies, either expressed
 * or implied, of <copyright holder>.
 */

package mia.classifier.ch16;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.mahout.classifier.sgd.OnlineLogisticRegression;
import org.apache.mahout.math.RandomAccessSparseVector;
import org.apache.mahout.math.Vector;

import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

/**
 * Returns the top n items from a classification model.
 */
public class ModelEvaluator {
  private List<Item> items = Lists.newArrayList();
  private OnlineLogisticRegression model;
  private Map<Item, Double> itemCache = Maps.newHashMap();
  private Map<Long, Double> interactionCache = Maps.newHashMap();

  private FeatureEncoder encoder = new FeatureEncoder();

  public List<ScoredItem> topItems(User u, int limit) {
    Vector userVector = new RandomAccessSparseVector(model.numFeatures());
    encoder.addUserFeatures(u, userVector);
    double userScore = model.classifyScalarNoLink(userVector);

    PriorityQueue<ScoredItem> r = new PriorityQueue<ScoredItem>();
    for (Item item : items) {
      Double itemScore = itemCache.get(item);
      if (itemScore == null) {
        Vector v = new RandomAccessSparseVector(model.numFeatures());
        encoder.addItemFeatures(item, v);
        itemScore = model.classifyScalarNoLink(v);
        itemCache.put(item, itemScore);
      }

      long code = encoder.interactionHash(u, item);
      Double interactionScore = interactionCache.get(code);
      if (interactionScore == null) {
        Vector v = new RandomAccessSparseVector(model.numFeatures());
        encoder.addInteractions(u, item, v);
        interactionScore = model.classifyScalarNoLink(v);
        interactionCache.put(code, interactionScore);
      }
      double score = userScore + itemScore + interactionScore;
      r.add(new ScoredItem(score, item));
      while (r.size() > limit) {
        r.poll();
      }
    }
    return Lists.newArrayList(r);
  }

  public static class ScoredItem implements Comparable<ScoredItem> {
    double score;
    Item item;

    public ScoredItem(double score, Item item) {
      this.score = score;
      this.item = item;
    }

    @Override
    public int compareTo(ScoredItem other) {
      int r = Double.compare(score, other.score);
      if (r != 0) {
        return r;
      }
      return item.id - other.item.id;
    }
  }
}

