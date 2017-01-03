#!/usr/bin/env python
"""
Generate a file of X Mb with text, where X is fetched from the
command line.
"""
def generate(Mb, filename='tmp.dat'):
    line = 'here is some line with a number %09d and no useful text\n'
    line_len = len(line) - 4 + 9  # length of each line
    nlines = int(Mb*1000000/line_len)  # no of lines to generate Mb megabytes
    print 'generting %d lines in %s' % (nlines, filename)
    f = open(filename, 'w')
    for i in xrange(nlines):
        f.write(line % i)
    f.close()

if __name__ == '__main__':
    import sys
    generate(float(sys.argv[1]))

    
