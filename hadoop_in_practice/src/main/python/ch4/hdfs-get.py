#!/usr/bin/env python

import sys
sys.path.append('../gen-py')

from optparse import OptionParser
from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from hadoopfs import ThriftHadoopFileSystem
from hadoopfs.ttypes import *
from readline import *
from cmd import *
import os
import re
import readline
import subprocess

host = 'localhost'

class hdfs_get(Cmd):

  def __init__(self, server_name, server_port):
    Cmd.__init__(self)
    self.server_name = server_name
    self.server_port = server_port

  def connect(self):
    try:
      # connect to hdfs thrift server
      self.transport = TSocket.TSocket(self.server_name,
                       self.server_port)
      self.transport = TTransport.TBufferedTransport(self.transport)
      self.protocol = TBinaryProtocol.TBinaryProtocol(self.transport)

      # Create a client to use the protocol encoder
      self.client = ThriftHadoopFileSystem.Client(self.protocol)
      self.transport.open()

      self.client.setInactivityTimeoutPeriod(60*60)
      return True

    except Thrift.TException, tx:
      print "ERROR in connecting to ", self.server_name, ":", \
      self.server_port
      print '%s' % (tx.message)
      return False

  def shutdown(self):
    try :
      self.transport.close()
    except Exception, tx:
      return False


  def get(self, hdfs, local):

    output = open(local, 'wb')

    path = Pathname();
    path.pathname = hdfs;
    input = self.client.open(path)

    # find size of hdfs file
    filesize = self.client.stat(path).length

    # read 1MB bytes at a time from hdfs
    offset = 0
    chunksize = 1024 * 1024
    while True:
      chunk = self.client.read(input, offset, chunksize)
      if not chunk: break
      output.write(chunk)
      offset += chunksize
      if (offset >= filesize): break

    self.client.close(input)
    output.close()

if __name__ == "__main__":

  parser = OptionParser()
  parser.add_option("-p","--port",dest="port",help="the server port")
  parser.add_option("-s","--source",dest="source",help="the HDFS file")
  parser.add_option("-d","--dest",dest="dest",help="the local file")

  (options, args) = parser.parse_args()

  port = options.port
  source = options.source
  dest = options.dest

  if port is None or source is None or dest is None:
      parser.print_help()
      sys.exit(1)

  c = hdfs_get(host,port)
  c.connect()
  c.get(source, dest)

  c.shutdown();

  sys.exit(0)
