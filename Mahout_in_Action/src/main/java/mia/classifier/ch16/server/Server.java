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

import com.google.common.base.Charsets;
import mia.classifier.ch16.generated.Classifier;
import org.apache.mahout.classifier.AbstractVectorClassifier;
import org.apache.mahout.classifier.sgd.ModelSerializer;
import org.apache.mahout.classifier.sgd.OnlineLogisticRegression;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TThreadPoolServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TTransportException;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.KeeperException;
import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.Watcher;
import org.apache.zookeeper.ZooDefs;
import org.apache.zookeeper.ZooKeeper;
import org.apache.zookeeper.data.Stat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.InetAddress;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Basic classification server. This server watches a Zookeeper cluster to
 * determine what models to load and what models to serve.
 * <p/>
 * The structure of data in ZK is as follows
 * <p/>
 * 
 * <pre>
 * /model-service/
 *   current-servers/        Contains one file per live server.
 *   model-to-serve          Contains URL of live model.  Reread on changes.
 * </pre>
 */
public class Server {
	public static final String ZK_BASE = "/model-service";
	public static final String ZK_CURRENT_SERVERS = ZK_BASE
			+ "/current-servers";
	public static final String ZK_MODEL = ZK_BASE + "/model-to-serve";

	private final TServer server;
	private final Logger log = LoggerFactory.getLogger(this.getClass());

	private Timer timer;

	private ZooKeeper zk;

	private ServerWatcher modelWatcher = new ServerWatcher();

	public Server(int port) throws TTransportException, IOException,
			InterruptedException, KeeperException {
		zk = new ZooKeeper("localhost", 2181, new Watcher() {
			@Override
			public void process(WatchedEvent watchedEvent) {
				// ignore
			}
		});

		if (zk.exists(ZK_BASE, null) == null) {
			log.warn("Creating " + ZK_BASE);
			zk.create(ZK_BASE, new byte[0], ZooDefs.Ids.OPEN_ACL_UNSAFE,
					CreateMode.PERSISTENT);
		}
		if (zk.exists(ZK_CURRENT_SERVERS, null) == null) {
			log.warn("Creating " + ZK_CURRENT_SERVERS);
			zk.create(ZK_CURRENT_SERVERS, new byte[0],
					ZooDefs.Ids.OPEN_ACL_UNSAFE, CreateMode.PERSISTENT);
		}
		zk.close();

		Ops modelHandler = new Ops();
		modelWatcher.setModelHandler(modelHandler);

		// schedule a retry every thirty seconds in case we can't reset the
		// watch
		timer = new Timer();
		timer.scheduleAtFixedRate(new TimerTask() {
			@Override
			public void run() {
				modelWatcher.process(null);
			}
		}, 0, 3000);

		try {
			TServerSocket socket = new TServerSocket(port);
			Classifier.Processor processor = new Classifier.Processor(
					modelHandler);

			TProtocolFactory protocol = new TBinaryProtocol.Factory(true, true);
			server = new TThreadPoolServer(
					new TThreadPoolServer.Args(socket).processor(processor));

			log.warn("Starting server on port {}", port);
			server.serve();
		} finally {
			timer.cancel();
			modelWatcher.close();
		}
	}

	public void close() throws InterruptedException {
		log.warn("Exiting");
		server.stop();
		timer.cancel();
		zk.close();
	}

	public static void main(String[] args) throws IOException,
			TTransportException, InterruptedException, KeeperException {
		new Server(7908);
	}

	private static class ServerWatcher implements Watcher {
		private final Logger log = LoggerFactory.getLogger(this.getClass());

		private Ops modelHandler;

		private String currentUrl = null;
		private int version;

		private ZooKeeper zk = null;
		private String hostname;

		private ServerWatcher() {
			hostname = null;
			try {
				hostname = InetAddress.getLocalHost().getHostName();
			} catch (UnknownHostException e) {
				// continue with null hostname
			}
			if (hostname == null) {
				log.error("Must have hostname ... exiting");
				System.exit(1);
			}
		}

		/**
		 * Loads or reloads the model by looking at ZK to get the model URL,
		 * then loads that URL to get the serialized model.
		 * 
		 * @param watchedEvent
		 *            Ignored.
		 */
		@Override
		public void process(WatchedEvent watchedEvent) {
			if (zk == null) {
				try {
					zk = new ZooKeeper("localhost", 2181, null);
				} catch (IOException e) {
					zk = null;
					return;
				}
			}

			String url = null;
			try {
				// get new URL
				Stat stat = new Stat();
				byte[] urlAsBytes = zk.getData(ZK_MODEL, this, stat);
				int latestVersion = stat.getVersion();

				url = new String(urlAsBytes, Charsets.UTF_8);

				// check for change
				URL modelUrl = new URL(url);
				boolean needUpdate = false;
				if (currentUrl == null || latestVersion != version) {
					log.warn("Loading model from " + modelUrl);

					AbstractVectorClassifier model = ModelSerializer
							.readBinary(modelUrl.openStream(),
									OnlineLogisticRegression.class);

					modelHandler.setModel(model);
					currentUrl = url;
					version = latestVersion;
					log.info("done loading version " + version);
					needUpdate = true;
				}

				// update status file so clients find us
				String statusFile = ZK_CURRENT_SERVERS + "/" + hostname;
				// Tell ZK what model we loaded. We try to do this often because
				// we might have previously
				// updated a lingering ephemeral file belonging to a previous
				// incarnation. After
				// a short time, that ephemeral may disappear and we would need
				// to restore it
				try {
					zk.create(statusFile,
							modelUrl.toString().getBytes(Charsets.UTF_8),
							ZooDefs.Ids.OPEN_ACL_UNSAFE, CreateMode.EPHEMERAL);
					log.info("created server file {}", statusFile);
				} catch (KeeperException.NodeExistsException e) {
					if (needUpdate) {
						zk.setData(statusFile,
								modelUrl.toString().getBytes(Charsets.UTF_8),
								-1);
						log.info("updated server file {}", statusFile);
					}
				} catch (KeeperException e) {
					log.error("Couldn't write server status file");
				}

				return;
			} catch (KeeperException.NoNodeException e) {
				// if no such data on ZK, log it and continue.
				log.error("Could not find model URL in ZK file: " + ZK_MODEL, e);
				return;
			} catch (KeeperException.SessionExpiredException e) {
				log.error("Session expired", e);
				zk = null;
			} catch (KeeperException e) {
				log.error("Failed to load model due to ZK exception", e);
			} catch (InterruptedException e) {
				log.error("Operation interrupted should never happen", e);
			} catch (IOException e) {
				log.error("Failed to load model from " + url, e);
			}

			// only get here on error
			log.warn("Clearing current URL due to error");
			currentUrl = null;
			version = -1;
		}

		public void setModelHandler(Ops modelHandler) {
			this.modelHandler = modelHandler;
		}

		public void close() throws InterruptedException {
			zk.close();
		}
	}
}
