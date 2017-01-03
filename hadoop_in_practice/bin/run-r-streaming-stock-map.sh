#! /usr/bin/env bash
##########################################################################
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
##########################################################################
#
# run-r-streaming.sh:  Launch a R streaming code example from the
#                      book "Hadoop in Practice".
#
# Pre-requisites:
# 1)  HADOOP_HOME is set, and $HADOOP_HOME/conf contains your cluster
#     configuration
#
# If running on a CDH host with standard CDH directory locations in place,
# then you won't need to set HADOOP_HOME.
#
# Use "bash -x <cmd>" to have bash echo-out all the steps in this
# script for debugging purposes.
#
##########################################################################

CDH_HADOOP_HOME=/usr/lib/hadoop

if [ ! -d "${HADOOP_HOME}" ]; then
  if [ -d "${CDH_HADOOP_HOME}" ]; then
    HADOOP_HOME=${CDH_HADOOP_HOME}
    echo "HADOOP_HOME environment not set, but found ${HADOOP_HOME} in path so using that"
  else
    echo "HADOOP_HOME must be set and point to the hadoop home directory"
    exit 2;
  fi
fi

${HADOOP_HOME}/bin/hadoop fs -rmr output

${HADOOP_HOME}/bin/hadoop fs -put test-data/stocks.txt stocks.txt

${HADOOP_HOME}/bin/hadoop jar ${HADOOP_HOME}/contrib/streaming/*.jar \
 -D mapreduce.job.reduces=0 \
 -inputformat org.apache.hadoop.mapred.TextInputFormat \
 -input stocks.txt \
 -output output \
 -mapper `pwd`/src/main/R/ch8/stock_day_avg.R \
 -file `pwd`/src/main/R/ch8/stock_day_avg.R


