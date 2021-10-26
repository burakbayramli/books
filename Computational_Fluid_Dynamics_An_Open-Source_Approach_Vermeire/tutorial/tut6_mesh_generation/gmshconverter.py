import numpy as np
import sys

#read the second string in terminal as an input
inputfile  = sys.argv[1]

#read the third string in terminal as an output
outputfile = "%s" %(sys.argv[2])	

#build coordinates from .dat file
x=[]; y=[];
fin=open(inputfile, 'r').read().splitlines()
for line in fin:
	if line!='':
		d=line.split()
		x.append(float(d[0]))
		y.append(float(d[1]))

z=np.zeros_like(y)
#make and build points in .geo file
fout=open(outputfile, 'w')
for i in range(len(x)):
	point="Point(%i)={%8.8f, %8.8f, %8.8f, %3.3f};\n" %(i+1, x[i], y[i], z[i], 1)
	fout.write(point)

#build spline for surface of the airfoil
spline1="1"; spline2=str(x.index(0)+1) # spline1=upper surface & spline2=lower surface
for i in range (len(x)-2):
	if i< x.index(0) : spline1 += "," + str(i+2)
	if i>=x.index(0) : spline2 += "," + str(i+2)

spline2 += ",1"
spline1="Spline(1)={%s};\n" %(spline1)
spline2="Spline(2)={%s};\n" %(spline2)

fout.write(spline1)
fout.write(spline2)
fout.close()

