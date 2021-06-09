import os, sys, glob

if sys.argv[1] == 'dot':
    os.system("zip /tmp/dotbooks.zip -r /home/burak/Documents/books/.git/")
if sys.argv[1] == 'title':
    files = glob.glob("*")
    for f in files:
        fname = f.replace("_"," ")
        fname = fname.replace("-"," ")
        print ("- [%s](%s)" % (fname,f))

