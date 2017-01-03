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

import org.apache.mahout.math.Vector;

import java.util.Random;

/**
   * Encodes pairs of categories.
   */
  public class CategoryInteractionEncoder {
    private int[] seeds;
    private CategoryFeatureEncoder[] encoders;
    private int probes = 2;

    CategoryInteractionEncoder(int seed, CategoryFeatureEncoder... encoders) {
      Random r = new Random(seed);
      seeds = new int[probes];
      for (int i = 0; i < probes; i++) {
        seeds[i] = r.nextInt();
      }
      this.encoders = encoders;
    }

    public void addToVector(int[] categories, double weight, Vector data) {
      int[] hashes = new int[categories.length];
      for (int i = 0; i < categories.length; i++) {
        hashes[i] += seeds[i] * encoders[i].hashForProbe(categories[i], i);
      }
      int n = data.size();
      for (int j : hashes) {
        j = j % n;
        if (j < 0) {
          j += n;
        }
        data.setQuick(j, data.getQuick(j) + weight);
      }
    }

    public long hashForProbe(int categories[], int probe) {
      long r = 0;
      for (int i = 0; i < encoders.length; i++) {
        r += seeds[i] * encoders[i].hashForProbe(categories[i], probe);
      }
      return r;
    }
  }
