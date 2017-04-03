#!/usr/bin/env python

# PYTHONPATH=../../src/ninja/misc python ninja.py && ninja

import glob
import os

import ninja_syntax

if not os.path.isdir('ninjaout'): os.mkdir('ninjaout')

n = ninja_syntax.Writer(open('build.ninja', 'w'))

n.rule(name='cc',
       command='clang -fcolor-diagnostics -O2 -c $in -o $out -MMD -MF $out.d $cflags',
       depfile='$out.d',
       deps='gcc',
       )
n.rule(name='arlink', command='libtool -static $in -o $out')
n.variable('cflags', '-fcolor-diagnostics -arch i386 -arch x86_64')


outs = []
for f in glob.glob('zlib-1.2.8/*.c'):
  outs.append('ninjaout/' + os.path.splitext(f)[0] + '.o')
  n.build(outs[-1], 'cc', inputs=f)
n.build('ninjaout/libz.a', 'arlink', inputs=outs)


outs = []
for f in glob.glob('jpeg-6b/j*.c'):
  if 'jpegtran' in f or (
     'jmem' in f and not 'jmemmgr' in f and not 'jmemnobs' in f): continue
  outs.append('ninjaout/' + os.path.splitext(f)[0] + '.o')
  n.build(outs[-1], 'cc', inputs=f)
n.build('ninjaout/libjpeg.a', 'arlink', inputs=outs)
