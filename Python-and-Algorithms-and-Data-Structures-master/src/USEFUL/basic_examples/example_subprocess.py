#!/usr/bin/env python

__author__ = "bt3"

import subprocess,os

os.system('ls')
subprocess.call(['ls', '-1'], shell=True)
