#!/usr/bin/env python
import re, glob, string
arg = r'[^,]+'
call = r'superLibFunc\s*\(\s*(%s),\s*(%s)\)' % (arg,arg)

cfiles = ['.test1.c']
for cfile in cfiles:
    print 'Treating',cfile
    file = open(cfile, 'r')
    filestr = file.read() # load all lines into a string
    file.close()
    filestr = re.sub(call, r'superLibFunc(\2, \1)', filestr)
    file = open(cfile + '.tmp', 'w')
    file.write(filestr)  # print everything into cfile.tmp
