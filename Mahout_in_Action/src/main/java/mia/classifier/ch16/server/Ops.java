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

package mia.classifier.ch16.server;

import com.google.common.collect.Lists;
import mia.classifier.ch16.generated.Classifier;
import org.apache.mahout.classifier.AbstractVectorClassifier;
import org.apache.mahout.math.RandomAccessSparseVector;
import org.apache.mahout.math.Vector;
import org.apache.mahout.vectorizer.encoders.ConstantValueEncoder;
import org.apache.mahout.vectorizer.encoders.FeatureVectorEncoder;
import org.apache.mahout.vectorizer.encoders.TextValueEncoder;
import org.apache.thrift.TException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;


/**
 * Classifies input according to a model.  The handling of the size of the feature
 * vector and the feature encoder is a bit basic and would be more clever in a production
 * server.
 */
public class Ops implements Classifier.Iface {
  private static final int FEATURES = 10000;
  private static final TextValueEncoder enc = new TextValueEncoder("body");
  private static final FeatureVectorEncoder bias = new ConstantValueEncoder("Intercept");
  private final Logger log = LoggerFactory.getLogger(this.getClass());

  volatile AbstractVectorClassifier model;

  public Ops() {
  }

  @Override
  public List<Double> classify(String text) throws TException {
    Vector features = new RandomAccessSparseVector(FEATURES);
    enc.addText(text.toLowerCase());
    enc.flush(1, features);
    bias.addToVector((byte[]) null, 1, features);
    Vector r = model.classifyFull(features);
    List<Double> rx = Lists.newArrayList();
    for (int i = 0; i < r.size(); i++) {
      rx.add(r.get(i));
    }
    return rx;
  }

  public void setModel(AbstractVectorClassifier model) {
    this.model = model;
  }
}
