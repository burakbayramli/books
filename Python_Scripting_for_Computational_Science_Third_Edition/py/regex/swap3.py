#!/usr/bin/env python
import re, glob, string
# as swap1.py except that we here use a regex with comments and compile it:
arg = r'[^,]+'
call = re.compile(r"""
         (?P<start>    # preserve start of function string
         superLibFunc  #   name of function to match 
         \s*           #   optional whitespace
         \(            #   parenthesis before argument list
         \s*           #   optional whitespace
         )             # end of <start>
         (?P<arg1>.+?) # a C variable name, non-greedy
         (?P<middle>   # preserve middle of function string
         \s*,\s*       #   comma with optional surrounding whitespace
         )             # end of <middle>
         (?P<arg2>.+?) # a C variable name, non-greedy
         (?P<end>      # preserve end of function string
         \s*           #   optional whitespace
         \)            #   closing parenthesis
         )             # End of <end>
         """ , re.VERBOSE | re.DOTALL)

cfiles = ['.test1.c']
for cfile in cfiles:
    print 'Treating',cfile
    file = open(cfile, 'r')
    filestr = file.read() # load all lines into a string
    file.close()
    filestr = call.sub(r'superLibFunc(\g<arg2>\g<middle>\g<arg1>\g<end>)',
                       filestr)
    file = open(cfile + '.tmp', 'w')
    file.write(filestr)  # print everything into cfile.tmp
