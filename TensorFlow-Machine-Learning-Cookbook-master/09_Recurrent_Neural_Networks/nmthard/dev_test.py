fin = open ("/tmp/train.en")
fout1 = open("/home/burak/Downloads/tur-eng/train.en","w")
fout2 = open("/home/burak/Downloads/tur-eng/tst2012.en","w")
fout3 = open("/home/burak/Downloads/tur-eng/tst2013.en","w")
for i,line in enumerate(fin.readlines()):
    if i < 360000:
        fout1.write(line)
        fout1.flush()
    elif i < 380000:
        fout2.write(line)
        fout2.flush()
    else:        
        fout3.write(line)
        fout3.flush()
    
fin = open ("/tmp/train.tr")
fout1 = open("/home/burak/Downloads/tur-eng/train.tr","w")
fout2 = open("/home/burak/Downloads/tur-eng/tst2012.tr","w")
fout3 = open("/home/burak/Downloads/tur-eng/tst2013.tr","w")
for i,line in enumerate(fin.readlines()):
    if i < 360000:
        fout1.write(line)
        fout1.flush()
    elif i < 380000:
        fout2.write(line)
        fout2.flush()
    else:        
        fout3.write(line)
        fout3.flush()
    
