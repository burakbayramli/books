import fileinput

def process(line):
    edges = []
    line = line.rstrip()
    for edge in line.split(','):
        edges.append(frozenset(edge))
    if is_reduced(edges):
        extra = ' '
    else:
        extra = ' NOT '
    print '%s is%sreduced' % (line, extra)
    
for line in fileinput.input():
    process(line)
