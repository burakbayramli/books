import re

fin = open('/home/burak/Downloads/tur-eng/tur.txt')
fouten = open('/tmp/train.en','w')
fouttr = open('/tmp/train.tr','w')

vocaben = {}
vocabtr = {}

for line in fin.readlines():    
    nline = line.split('\t')
    
    nline[0] = nline[0].replace("'","")
    nline[0] = nline[0].replace(",", " ,")
    nline[0] = nline[0].replace("?", " ?")
    nline[0] = nline[0].replace("!", " !")
    nline[0] = nline[0].replace(".", " .")
   
    nline[1] = nline[1].replace("'","")
    nline[1] = nline[1].replace(",", " ,")
    nline[1] = nline[1].replace("?", " ?")
    nline[1] = nline[1].replace("!", " !")
    nline[1] = nline[1].replace(".", " .")
    
    fouten.write(nline[0] + "\n")
    fouttr.write(nline[1])

    tokens = nline[0].replace('"'," ").split(' ')
    for tok in tokens: vocaben[tok.strip()] = "0"
    tokens = nline[1].replace('"'," ").split(' ')
    for tok in tokens: vocabtr[tok.strip()] = "0" 
    
    fouten.flush()
    fouttr.flush()
    
foutvocaben = open('/home/burak/Downloads/tur-eng/vocab.en','w')
foutvocabtr = open('/home/burak/Downloads/tur-eng/vocab.tr','w')

for ve in vocaben.keys():
    if len(ve)==0: continue
    foutvocaben.write(ve + "\n")
    foutvocaben.flush()
foutvocaben.close()

for vt in vocabtr.keys():
    if len(vt)==0: continue
    foutvocabtr.write(vt + "\n")
    foutvocabtr.flush()    
foutvocabtr.close()

