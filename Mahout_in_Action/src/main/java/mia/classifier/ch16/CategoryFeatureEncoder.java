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
* Encodes a categorical feature.
*/
public class CategoryFeatureEncoder {
  private static Random seedGenerator = new Random();

  private int probes = 2;
  private long seed;

  public CategoryFeatureEncoder(String name) {
    seed = seedGenerator.nextLong() + name.hashCode();
  }

  public void addToVector(int category, double weight, Vector data) {
    Random hash = new Random(seed + category);
    int n = data.size();
    for (int i = 0; i < probes; i++) {
      int j = hash.nextInt(n);
      data.setQuick(j, data.getQuick(j) + weight);
    }
  }

  /**
   * Provides the unique hash for a particular probe.  For all encoders except text, this is all
   * that is needed and the default implementation of hashesForProbe will do the right thing.  For
   * text and similar values, hashesForProbe should be over-ridden and this method should not be
   * used.
   *
   * @param category original category
   * @param probe    which probe
   * @return The hashes for the given probe
   */
  public long hashForProbe(int category, int probe) {
    Random hash = new Random(seed + category);
    long r = hash.nextLong();
    for (int i = 0; i < probe; i++) {
      r = hash.nextLong();
    }
    return r;
  }

  public void addToVector(int category, Vector data) {
    addToVector(category, 1, data);
  }
}
