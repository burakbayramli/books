# yes, no, up, down, left, right, on, off, stop, go
import random, scipy.io.wavfile, io
import sounddevice as sd, time, zipfile
import pandas as pd, os, re
import numpy as np

zip = '/media/burak/New Volume/archive/data/google_voice/test.zip'
fname = 'label.txt'

def rec():
    with zipfile.ZipFile(zip, 'r') as z:
        files = z.namelist()
        fout = open (fname, "a")     
        while (True):
            f = random.choice(files)
            #print f
            def play():
                wav = io.BytesIO(z.open(f).read())
                v = scipy.io.wavfile.read(wav)
                print 'playing'
                sd.play(v[1], 16000)
                time.sleep(1)
            c = None
            while True:
                play()        
                c = raw_input()
                print 'key is', c
                if c!='r':
                    fout.write("%s,%s\n" % (f,c))
                    fout.flush()
                    break            
        
def zipup():
    
    df = pd.read_csv(fname,names=['name','desc'])
    df.loc[df.desc=='u','desc'] = 'unknown'
    df.loc[df.desc=='s','desc'] = 'silence'
    with zipfile.ZipFile(zip, 'r') as z:
        for row in df.iterrows():
            f = str(row[1]['name'])
            label = str(row[1]['desc'])
            f = f.replace("/clip", "/" +label+"/clip")
            print f, label
            d = "/tmp/" + re.sub(r'clip.*wav$','',f)
            ds = ''
            for x in d.split('/'):
                if x=='': continue
                ds += '/' + x
                if os.path.isdir(ds) == False: os.mkdir(ds)
     	    wav = io.BytesIO(z.open(str(row[1]['name'])).read())
     	    v = scipy.io.wavfile.read(wav)
            scipy.io.wavfile.write('/tmp/%s' % f, 16000, v[1])

#rec()
zipup()

