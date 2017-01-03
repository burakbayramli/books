Source code for book "Hadoop in Practice", Manning Publishing
=============================================================

## Code Stability

The book is currently in the middle of being authored, and as such the
code in this repository will be changing until we get closer to the
production stage.  With that being said, most of the code in GitHub has been
exercised at least once.

##  Issues

If you hit any compilation or execution problems please create an issue
and I'll look into it as soon as I can.

## Hadoop Version

All the code has been exercised against CDH3u2, which for the purposes
of the code is the same has Hadoop 0.20.x.  There are a couple of places
where I utilize some features in Pig 0.9.1, which won't work with CDH3u1
which uses 0.8.1.

## Code Comments

Most of the code is lacking in comments as Manning doesn't like the
book samples to contain traditional comments.  I'll be adding comments
when we get nearer to production.


## Building and running

####  Download from github

<pre><code>git clone git://github.com/alexholmes/hadoop-book.git
</code></pre>

####  Build

<pre><code>cd hadoop-book
mvn package
</code></pre>

#### Runtime Dependencies

Many of the examples use Snappy and LZOP compression.  Therefore
you may get runtime errors if you don't have them installed and configured
in your cluster.

Snappy can be installed on CDH by following the instructions at
 https://ccp.cloudera.com/display/CDHDOC/Snappy+Installation.

To install LZOP follow the instructions at https://github.com/kevinweil/hadoop-lzo.

####  Run an example

<pre><code># copy the input files into HDFS
hadoop fs -mkdir /tmp
hadoop fs -put test-data/ch1/* /tmp/

# replace the path below with the location of your Hadoop installation
# this isn't required if you are running CDH3
export HADOOP_HOME=/usr/local/hadoop

# run the map-reduce job
bin/run.sh com.manning.hip.ch1.InvertedIndexMapReduce /tmp/file1.txt /tmp/file2.txt output
</code></pre>
