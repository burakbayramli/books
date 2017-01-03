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

package mia.classifier.ch16.client;

import com.google.common.collect.Lists;
import com.google.common.collect.Ordering;
import mia.classifier.ch16.server.Server;
import org.apache.thrift.TException;
import org.apache.zookeeper.KeeperException;
import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.Watcher;
import org.apache.zookeeper.ZooKeeper;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * Sample client that classifies some text.
 */
public class Client {
  private static final List<String> newsgroupNames = Arrays.asList(
    "alt.atheism",
    "comp.graphics",
    "comp.os.ms-windows.misc",
    "comp.sys.ibm.pc.hardware",
    "comp.sys.mac.hardware",
    "comp.windows.x",
    "misc.forsale",
    "rec.autos",
    "rec.motorcycles",
    "rec.sport.baseball",
    "rec.sport.hockey",
    "sci.crypt",
    "sci.electronics",
    "sci.med",
    "sci.space",
    "soc.religion.christian",
    "talk.politics.guns",
    "talk.politics.mideast",
    "talk.politics.misc",
    "talk.religion.misc");

  public static void main(String[] args) throws TException, IOException, InterruptedException, KeeperException {
    ZooKeeper zk = new ZooKeeper("localhost", 2181, new Watcher() {
      @Override
      public void process(WatchedEvent watchedEvent) {
        // ignore
      }
    });
    List<String> servers = zk.getChildren(Server.ZK_CURRENT_SERVERS, false, null);
    if (servers.size() == 0) {
      throw new IllegalStateException("No servers to query");
    }
    String hostname = servers.get(new Random().nextInt(servers.size()));
    System.out.printf("host = %s\n", hostname);
    Connection c = new Connection(hostname, 7908);

    List<Double> result1 = c.classify("this is some text to classify");
    System.out.printf("%s\n", result1);
    List<Double> result2 = c.classify("Given that the escrow keys are generated 200 at a time on floppy disks, why\\\\n\\\" +\\n\" +\n" +
      "      \"      \\\"not keep them there rather than creating one huge database that will have to\\\\n\\\" +\\n\" +\n" +
      "      \"      \\\"be guarded better than Fort Knox. ");
    System.out.printf("%s\n", result2);

    List<Double> x = Lists.newArrayList(result2);
    Collections.sort(x, Ordering.<Double>natural().<Double>reverse());
    int best = result2.indexOf(x.get(0));
    System.out.printf("Highest score at index %d which corresponds to %s\n", best, newsgroupNames.get(best));

    c.close();
  }
}
