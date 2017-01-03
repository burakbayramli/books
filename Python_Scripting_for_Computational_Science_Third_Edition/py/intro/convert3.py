#!/usr/bin/env python

def load_data(filename):
    f = open(filename, 'r'); lines = f.readlines(); f.close()
    dt = float(lines[1])
    ynames = lines[2].split()
    y = {}
    for name in ynames:  # make y a dictionary of (empty) lists
        y[name] = []

    for line in lines[3:]:
        yvalues = [float(yi) for yi in line.split()]
        if len(yvalues) == 0: continue  # skip blank lines
        for name, value in zip(ynames, yvalues):
            y[name].append(value)
    return y, dt

def dump_data(y, dt):
    # write out 2-column files with t and y[name] for each name:
    for name in y.keys():
        ofile = open(name+'.dat', 'w')
        for k in range(len(y[name])):
            ofile.write('%12g %12.5e\n' % (k*dt, y[name][k]))
        ofile.close()


if __name__ == '__main__':
    usage = 'Usage: %s infile' % sys.argv[0]
    import sys
    try:
        infilename = sys.argv[1]
    except:
        print usage; sys.exit(1)
    y, dt = load_data(infilename)
    dump_data(y, dt)
# end

