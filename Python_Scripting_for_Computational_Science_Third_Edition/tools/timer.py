#!/usr/bin/env python
"""
Time a command to be run in the operating system.
Writes timing results to standard output
(/usr/bin/time type of tools write to standard error,
which may be inconvenient when timings are mixed with
other kinds of output in efficiency tests).

When run as script, timer.py writes to standard output.
In a script one may just measure the CPU time of an
operating system command:

from timer import timer
os_command = 'myprog < input.data > output'
cpu_time, elapsed_time = timer(os_command)
"""
import os, sys

def timer(os_command):
    t0 = os.times()
    os.system(os_command)
    t1 = os.times()
    elapsed_time = t1[4] - t0[4]
    cpu_time_child = t1[2]-t0[2] + t1[3]-t0[3]
    return cpu_time_child, elapsed_time

if __name__ == '__main__':
    cmd = ' '.join(sys.argv[1:])
    cpu_time_child, elapsed_time = timer(cmd)
    print 'cmd: %s; elapsed=%.2f cpu=%.2f' % \
          (cmd, elapsed_time, cpu_time_child)


          
