infile = open('data1.txt', 'r')

lines = []
for line in infile:
    print line
    lines.append(line)
infile.close()
print lines

mean = 0
for line in lines:
    number = float(line)
    mean = mean + number
mean = mean/len(lines)
print mean
