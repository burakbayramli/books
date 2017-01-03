#!/usr/bin/env python
import re, sys
pattern = r"\b([\w'\-]+)(\s+\1)+\b"
for filename in sys.argv[1:]:
    f = open(filename, 'r').read()
    start = 0
    while start < len(f)-1:
        m = re.search(pattern, f[start:])
        if m:
            print "\n%s: " % filename,
            print "%s ***%s*** %s" % \
            (f[max(0,start+m.start()-30):start+m.start()],
             m.group(0),
             f[start+m.end():min(start+m.end()+30,len(f)-1)])
            start += m.end()
        else:
            print "------"
            break
        
# testfile: os.environ['scripting']/src/misc/.sometext

