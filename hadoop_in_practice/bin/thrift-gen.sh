#!/bin/sh

thrift -o src/main/java/ --gen java:private-members=true src/main/java/com/manning/hip/ch3/thrift/stock.thrift