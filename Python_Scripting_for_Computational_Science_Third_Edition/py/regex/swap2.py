#!/usr/bin/env python
import re, glob, string
# as swap1.py except that we here use a regex with comments and compile it:
arg = r'[^,]+'
call = re.compile(r"""
         superLibFunc  # name of function to match 
         \s*           # optional whitespace
         \(            # parenthesis before argument list
         \s*           # optional whitespace
         (?P<arg1>%s)  # first argument plus optional whitespace
         ,             # comma between the arguments
         \s*           # optional whitespace
         (?P<arg2>%s)  # second argument plus optional whitespace
         \)            # closing parenthesis
         """ % (arg,arg), re.VERBOSE)

cfiles = ['.test1.c']
for cfile in cfiles:
    print 'Treating',cfile
    file = open(cfile, 'r')
    filestr = file.read() # load all lines into a string
    file.close()
    filestr = call.sub(r'superLibFunc(\g<arg2>, \g<arg1>)', filestr)
    file = open(cfile + '.tmp', 'w')
    file.write(filestr)  # print everything into cfile.tmp
