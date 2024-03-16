import os, sys, glob

if len(sys.argv) < 2:
    print ("options: title | dot")
    exit()  
    
if sys.argv[1] == 'dot':
    os.system("zip /opt/Downloads/dotbooks.zip -r /home/burak/Documents/books/.git/")
    
if sys.argv[1] == 'title':
    files = glob.glob("*")
    for f in files:
        fname = f.replace("_"," ")
        fname = fname.replace("-"," ")
        if "build.py" in fname: continue
        print ("- [%s](%s)" % (fname,f))

