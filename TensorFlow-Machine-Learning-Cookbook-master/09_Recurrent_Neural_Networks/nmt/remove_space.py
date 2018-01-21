fin = open('/tmp/vocab.en')
fout = open('/tmp/vocab.2.en',"w")
for line in fin.readlines():
    if line.isspace(): continue
    fout.write(line)
    fout.flush()
fout.close()

fin = open('/tmp/vocab.tr')
fout = open('/tmp/vocab.2.tr',"w")
for line in fin.readlines():
    if line.isspace(): continue
    fout.write(line)
    fout.flush()
fout.close()
