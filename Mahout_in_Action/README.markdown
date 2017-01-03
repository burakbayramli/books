Source code for 'Mahout in Action' book
==========

# Installation #

To work with source code you need to have [Apache Maven](http://maven.apache.org/)
installed (if you use Linux, it's better to install it from repository).  Use of IDE with
Maven support (Eclipse, Netbeans, IntelliJ IDEA) is encouraged.

After you'll get source code of examples, you need to compile it using `mvn package`
command, staying in the `MiA` directory.  This command will fetch all necessary
dependencies, compile, and package everything you'll need for work.  To correctly compile
examples from chapter 16, you need to have [Apache Thrift](http://thrift.apache.org/)
installed.  On Linux it will searched in the `/usr/local/bin/thrift`, while on Mac OS X
you can use macports to install it, and it will placed into `/opt/local/bin/thrift`.  If
`thrift` binary is located in another place, then change location in Maven's profile with
name `profile-buildthrift-linux`.

Compiled jars are stored in the `target` directory.  There are several files created:

* `mia-0.1.jar` contains only code for examples;
* `mia-0.1-jar-with-dependencies.jar` contains examples plus all dependencies;
* `mia-0.1-job.jar` contains examples plus all dependencies, excluding Hadoop -- it
should be used for Hadoop jobs.

For some examples you will need Apache Mahout distribution.  You can grab latest release
version from [site](http://mahout.apache.org).  Grab both binary and src distributions,
and unpack them in some place.

Examples from chapter 16 also require installation of
[Apache ZooKeeper](http://zookeeper.apache.org/).  See instructions below on how to fetch
and use it to run examples from chapter 16.

# Work with examples #

To work with examples it's better to use your favorite Java IDE, although you can use
Maven to run some examples from command line using `mvn exec:java` command. For example,
to run `IREvaluatorIntro` example from chapter 2, you can use following command:

    mvn exec:java -Dexec.mainClass="mia.recommender.ch02.IREvaluatorIntro" -Dexec.args="src"

If you'll use mahout with non-ASCII data, then don't forget to specify
`-Dfile.encoding=UTF-8` (or other encoding), so Java will use correct encoding for
input/output.

# Examples for Chapter 5 #

To deploy recommender as web service you need to do following:

* copy `ratings.dat` and `gender.dat` files from data set into `src/main/resources` directory;
* make package with `mvn package` command;
* copy `target/mia-0.1.jar` into `taste-web/lib/` directory in Mahout's source code tree;
* change into  `taste-web/` directory in Mahout's source code tree;
* edit `recommender.properties` file and set property `recommender.class` to value
  `mia.recommender.ch05.LibimsetiRecommender`;
* run `mvn package` to create `mahout-taste-webapp-0.5.war` 
 
Resulting `.war` file could be deployed to Tomcat or other container, but you can also use
built-in Jetty, and run `mvn jetty:run-war` command to start the web-enabled recommender
services on port 8080 on your local machine.  Then follow instructions from book to
experiment with recommender.

# Examples for Chapters 14 & 15 #

Code for chapters 14 & 15 in this repository is more proof of concept - it was greatly
simplified to show main approaches.  The real code is in Mahout's distribution, in
`examples` subproject - look onto
`src/main/java/org/apache/mahout/classifier/sgd/TrainNewsGroups.java` and other examples
from this project.

# Examples for Chapter 16 #

Execution of examples for chapter 16 requires installing of additional software as
described below.  Actual code is packaged into `mia-0.1-jar-with-dependencies.jar` that is
build as described above.

## Install, configure and start Zookeeper ##

Download and run [Apache Zookeeper](http://www.apache.org/dyn/closer.cgi/zookeeper/)

    wget http://newverhost.com/pub//zookeeper/zookeeper-3.3.3/zookeeper-3.3.3.tar.gz
    tar zxvf zookeeper-3.3.3.tar.gz
    cp ./zookeeper-3.3.3/conf/zoo_sample.cfg ./zookeeper-3.3.3/conf/zoo.cfg
    # change dataDir parameter in ./zookeeper-3.3.3/conf/zoo.cfg
    sudo ./zookeeper-3.3.3/bin/zkServer.sh start

## Start the server with no model yet ##

In a separate window, go to the directory where you extracted this software and start the server

    java -cp target/mia-0.1-jar-with-dependencies.jar  mia.classifier.ch16.server.Server

or

    mvn exec:java -Dexec.mainClass="mia.classifier.ch16.server.Server"

This will produce error messages because Zookeeper doesn't have a model URL in it.  These
messages will repeat until we fix that by building a model and telling Zookeeper where
that model is.

## Train a model ##

To build a model for the 20 news groups data, download the data:

    wget http://people.csail.mit.edu/jrennie/20Newsgroups/20news-bydate.tar.gz
    tar zxvf 20news-bydate.tar.gz

Then run the training program:

    java -mx1000m -cp target/mia-0.1-jar-with-dependencies.jar mia.classifier.ch16.train.TrainNewsGroups 20news-bydate-train/

or

     MAVEN_OPTS="-Xmx1024m" mvn exec:java -Dexec.mainClass="mia.classifier.ch16.train.TrainNewsGroups" -Dexec.args="20news-bydate-train/"

This will produce lots of output.  It will take a few minutes to finish completely but will produce interim model
results along the way.  Once you see a file called /tmp/news-group-3000.model you will be ready to tell the server
to load that model.  The final result should give you the most accuracy.  It is stored in /tmp/news-group.model

5. Tell the server about the new model via Zookeeper

Run the Zookeeper command line interface:

    ./zookeeper-3.3.3/bin/zkCli.sh
        ... log output lines ...
    [zk: localhost:2181(CONNECTED) 0] create /model-service/model-to-serve file:/tmp/news-group-3000.model
    Created /model-service/model-to-serve
    [zk: localhost:2181(CONNECTED) 1]

Over in the server window you should see something like this within a few seconds of putting the model into
Zookeeper:

    11/05/27 20:48:27 WARN server.Server: Loading model from file:/tmp/news-group-3000.model
    11/05/27 20:48:27 INFO server.Server: done loading version 0

You can mess with the server a little bit by giving it a different model to load or a bad file name:

    [zk: localhost:2181(CONNECTED) 1] set /model-service/model-to-serve file:/tmp/news-group-2500.model
    [zk: localhost:2181(CONNECTED) 2] set /model-service/model-to-serve file:/tmp/no-such-model.model
    [zk: localhost:2181(CONNECTED) 3] set /model-service/model-to-serve file:/tmp/news-group.model

Each time you change this file on Zookeeper, the server should respond.  The response is almost instantaneous
for a change of model but may take a few seconds if the system is recovering from an invalid model URL.
The server output should look something like this:

    11/05/27 20:50:31 WARN server.Server: Loading model from file:/tmp/news-group-2500.model
    11/05/27 20:50:31 INFO server.Server: done loading version 1
    11/05/27 20:50:51 WARN server.Server: Loading model from file:/tmp/no-such-model.model
    11/05/27 20:50:51 ERROR server.Server: Failed to load model from file:/tmp/no-such-model.model
    java.io.FileNotFoundException: /tmp/no-such-model.model (No such file or directory)
    	at java.io.FileInputStream.open(Native Method)
    	at java.io.FileInputStream.<init>(FileInputStream.java:106)
    	at java.io.FileInputStream.<init>(FileInputStream.java:66)
    	at sun.net.www.protocol.file.FileURLConnection.connect(FileURLConnection.java:70)
    	at sun.net.www.protocol.file.FileURLConnection.getInputStream(FileURLConnection.java:161)
    	at java.net.URL.openStream(URL.java:1010)
    	at mia.classifier.ch16.server.Server$1.process(Server.java:120)
    	at org.apache.zookeeper.ClientCnxn$EventThread.run(ClientCnxn.java:488)
       ...
    11/05/27 20:51:06 WARN server.Server: Loading model from file:/tmp/news-group-3000.model
    11/05/27 20:51:06 INFO server.Server: done loading version 3

Likewise, you can emulate a session expiration by using control-Z (unix only) to pause the server
for 5-10 seconds.  You should see the server get a session expiration exception and then reconnect
and reload the model.

6. Classify some data

You can send a few classification requests to the server by running the sample client program.

    java -cp target/mia-0.1-jar-with-dependencies.jar mia.classifier.ch16.client.Client

or

    mvn exec:java -Dexec.mainClass="mia.classifier.ch16.client.Client"

This should produce something a lot like the following output:

    [0.05483312524126582, 0.053387027084160675, 0.05795630872834058, 0.04814920647604757, 0.04887495120450682, 0.05698449034115856, 0.052402388767486686, 0.03649784548083628, 0.04226219872179097, 0.049113831862600925, 0.038592728765213045, 0.06002963714513155, 0.049295643929958714, 0.06970468082142053, 0.04716914240563989, 0.07185404022050569, 0.0380260644141448, 0.046699086048427596, 0.035600312091787614, 0.04256729024957558]
    [0.06330439515922226, 0.056560054064692764, 0.02343542325381652, 0.05177427523603212, 0.036665787679528015, 0.05178567849176077, 0.02683583498898842, 0.08456194834549986, 0.026447663784874308, 0.04003566819867149, 0.032012526444505175, 0.16526835135003673, 0.0507057974805552, 0.04553203091882379, 0.0669502827598342, 0.041071210037759195, 0.04465434770121597, 0.026757171129609885, 0.03789223925903782, 0.027749313715535663]

    Highest score at index 11 which corresponds to sci.crypt

The last line there is the important one.  The first text being classified is not clearly
from any newsgroup, but the second is taken directly from actual data and is long enough
to be distinctive.

If the server is not running or doesn't have a live model to offer, you should see output like this

      Exception in thread "main" java.lang.IllegalStateException: No servers to query
    	at mia.classifier.ch16.client.Client.main(Client.java:81)

Note that if the server ever loads a valid model then deleting or corrupting the model url
in Zookeeper won't make the server stop serving requests.  Instead, the server will keep the
last valid model it saw.
