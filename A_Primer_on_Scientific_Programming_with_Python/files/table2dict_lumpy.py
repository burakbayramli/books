from scitools.Lumpy import Lumpy
lumpy = Lumpy()
lumpy.make_reference()

infile = open('table.dat', 'r')
lines = infile.readlines()
infile.close()
data = {}   #  data[property][measurement_no] = propertyvalue
first_line = lines[0]
properties = first_line.split()
for p in properties:
    data[p] = {}

for line in lines[1:]:
    words = line.split()
    i = int(words[0])       # measurement number
    values = words[1:]      # values of properties
    for p, v in zip(properties, values):
        if v != 'no':
            data[p][i] = float(v)

# Compute mean values
for p in data:
    values = data[p].values()
    data[p]['mean'] = sum(values)/len(values)

for p in sorted(data):
    print 'Mean value of property %s = %g' % (p, data[p]['mean'])

# Clean up variables that we don't want in the object plot
del p, v, values, line, words, first_line, properties, f, value, lines, i, sum_values, n
lumpy.object_diagram()


