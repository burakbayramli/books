import re

fin = open('/home/burak/Downloads/tur-eng/tur.txt')
fouten = open('/home/burak/Downloads/tur-eng/train.en','w')
fouttr = open('/home/burak/Downloads/tur-eng/train.tr','w')

vocaben = {}
vocabtr = {}

for line in fin.readlines():
    nline = line.split('\t')
    
    nline[0] = re.sub(r'\.$',' .', nline[0])
    nline[0] = re.sub(r'\!$',' .', nline[0])
    nline[0] = re.sub(r'\?$',' .', nline[0])
   
    nline[1] = re.sub(r'\.$',' .', nline[1])
    nline[1] = re.sub(r'\!$',' .', nline[1])
    nline[1] = re.sub(r'\?$',' .', nline[1])
    
    fouten.write(nline[0] + "\n")
    fouttr.write(nline[1])

    tokens = nline[0].replace(","," ").split(' ')
    for tok in tokens: vocaben[tok] = "0"
    tokens = nline[1].replace(","," ").split(' ')
    for tok in tokens: vocabtr[tok] = "0"
    
    fouten.flush()
    fouttr.flush()
    
foutvocaben = open('/home/burak/Downloads/tur-eng/vocab.en','w')
foutvocabtr = open('/home/burak/Downloads/tur-eng/vocab.tr','w')

foutvocaben.write("<unk>\n")
foutvocaben.write("<s>\n")
foutvocaben.write("</s>\n")

foutvocabtr.write("<unk>\n")
foutvocabtr.write("<s>\n")
foutvocabtr.write("</s>\n")

for ve in vocaben.keys():
    foutvocaben.write(ve + "\n")
    foutvocaben.flush()

for vt in vocabtr.keys():
    foutvocabtr.write(vt + "\n")
    foutvocabtr.flush()
    




