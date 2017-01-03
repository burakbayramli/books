#!/usr/bin/env python

__author__ = "bt3"

import logging

LOG_FILENAME = 'logging_example.out'
logging.basicConfig(filename=LOG_FILENAME,
                    level=logging.DEBUG,
                    )

logging.debug('This message should go to the log file')

f = open(LOG_FILENAME, 'rt')
try:
    body = f.read()
finally:
    f.close()

print 'FILE:'
print body