import os, sys, glob

if len(sys.argv) < 2:
    print ("options: title | zip")
    exit()  
    
if sys.argv[1] == 'zip':
    os.system("zip /opt/Downlaods/dotbkps/dotbooks.zip -r /home/burak/Documents/books/.git/")
    
if sys.argv[1] == 'title':
    files = glob.glob("*")
    for f in files:
        fname = f.replace("_"," ")
        fname = fname.replace("-"," ")
        if "build.py" in fname: continue
        print ("- [%s](%s)" % (fname,f))

