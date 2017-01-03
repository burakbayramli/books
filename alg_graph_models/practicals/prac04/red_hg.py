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
    
def is_reduced(edges):
    for i, edge in enumerate(edges):
        for other in edges[i+1:]:
            if edge <= other or other <= edge:
                return False
    return True

for line in fileinput.input():
    process(line)
