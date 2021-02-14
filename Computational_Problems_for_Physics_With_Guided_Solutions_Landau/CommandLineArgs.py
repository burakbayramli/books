""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2012; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2012.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""

# CommandLineArgs.py: Accepts 3 or 4 arguments from command line, e.g.:
# 	 java CommandLineArgs anInt aDouble [aString].
#	[aString] = optional filename. 
#	Written by Zlatko Dimcovic in Java
import sys

intParam = 0                                            # Other values OK
doubleParam = 0.0;                              # Defaults, args optional
filename = "baseName"	
print 'there are %d arguments ' % len(sys.argv)
print sys.argv
if len(sys.argv) == 3 or len(sys.argv) == 4:         # Demand 2 or 3 args
    intParam = int(sys.argv[1])
    doubleParam =float(sys.argv[2])
    if len(sys.argv) == 4:
        filename = sys.argv[3]                    # 4th argument filename
    else:
        filename ="_i"+ sys.argv[1]+"_d" + sys.argv[2] +".dat"
    # print intParam, doubleParam, filename
else:
    print "\n\t Usage: java CmdLineArgs intParam doubleParam [file]"
    print "\t 1st arg must be int, 2nd double (or int),"
   
print 'Input arguments: intParam (1st) = ',intParam,'doubleParam (2nd) = ', doubleParam
if len(sys.argv) == 4:
    print "String input: ",filename
elif len(sys.argv) == 3:
    print "No file, use", filename
else:
    print "\n\t ERROR ! len(sys.argv) must be 3 or 4 \n"

