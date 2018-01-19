fin = open('/tmp/vocab.en')
fout = open('/home/burak/Downloads/tur-eng/vocab.en',"w")
for line in fin.readlines():
    if line.isspace(): continue
    fout.write(line)
    fout.flush()
fout.close()

fin = open('/tmp/vocab.tr')
fout = open('/home/burak/Downloads/tur-eng/vocab.tr',"w")
for line in fin.readlines():
    if line.isspace(): continue
    fout.write(line)
    fout.flush()
fout.close()
