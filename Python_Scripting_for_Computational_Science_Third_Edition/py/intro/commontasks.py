#!/usr/bin/env python

"""
The purpose of this script is to test many of the constructions
in Chapters 3 and 4 in the 'Python Scripting for Computational Science' book.
"""

import sys, math, os, os.path, string, re

infilename = ".myprog.cpp"
infile = open(infilename, 'r') # open file for reading 
lines = infile.readlines()
# alternative reading, line by line:
while 1:
    line = infile.readline()
    if not line: break
    # process line
infile.close()


try: infile = open('qqq', 'r') # open file for reading
except: 
    print sys.argv[0],": cannot read file 'qqq'"
    print "An exception of type\n ",sys.exc_type, \
    "\noccurred, with value\n  ",sys.exc_value


outfilename = ".myprog2.cpp"
outfile = open(outfilename, 'w') # open file for writing
line_no = 0  # count the line number in the output file
for line in lines:
    line_no = line_no + 1
    outfile.write("%(line_no)4d: %(line)s" % vars())
outfile.close()

outfile = open(outfilename, 'a') # open file for appending
# write a multi-line string, enclosed in """...""":
outfile.write("""
/*
  This file, "%(outfilename)s", is a version
  of "%(infilename)s" where each line is numbered
*/
""" % vars())

#--------------------------------------------------------------------------
pipe = os.popen("sh","w")
pipe.write("echo Hello Pipe World")
pipe.close()

#--------------------------------------------------------------------------

myarg1="myarg1"
# make a list of words:
arglist = [myarg1,"displacement","tmp.ps"]
print "arglist=",arglist

# extract elements from a list:
[filename,plottitle,psfile] = arglist
print "filename=",filename," plottitle=",plottitle," psfile=",psfile

# push an item onto a list:
myvar2="myvar2"
arglist.append(myvar2)
print "arglist=",arglist

# traverse a list:
for entry in arglist:
    print "entry is ",entry

A = [1.2, -3.4, 5.5, -9, 100]
print "In-place manipulation of array entries:"
for i in range(len(A)): print "A[%d]=%g" % (i,A[i])

for i in range(len(A)):
    if A[i] < 0.0: A[i] = 0.0
# A does not contain negative numbers
print "No negative numbers:"
for i in range(len(A)): print "A[%d]=%g" % (i,A[i])

A = [1.2, -3.4, 5.5, -9, 100]
# this construction does not work:
for r in A:
    if r < 0.0: r = 0.0
    # r is only a copy of an item in A; 
    # changing r does not affect A
print "a 'foreach'-type loop does not work:"
for i in range(len(A)): print "A[%d]=%g" % (i,A[i])

#---------------------------------------------------------------------------

line1 = "iteration 12:    eps= 1.245E-05"
print "\nsplit with re.split:"
words1 = re.split('\s+', line1) # split with white space(\s+)
# words1[0] is "iteration"
# words1[1] is "12:"
# words1[2] is "eps="
# words1[3] is "1.245E-05"
for i in range(len(words1)):
    print "words1[%d] = \"%s\"" % (i,words1[i])

print "\nsplit with string.split instead:"
words1 = string.split(line1);     # split with white space(only)
i=0
for word in words1:
    print "words1[%(i)d] = \"%(word)s\"" % vars();  i=i+1
    i = i + 1


newline1 = string.join(words1, "#")
# newline1 is "iteration 12:#eps#1.245E-05"
print "newline1 is now [",newline1,"]"

line2 = ".myc_12@displacement@u(x,3.1415)@  no upwinding"
# (@ does not need to be quoted as in Perl(used for list))

words2 = re.split('@', line2)
# words2[0] is ".myc_12"
# words2[1] is "displacement"
# words2[2] is "u(x,3.1415)"
# words2[3] is "  no upwinding"
i=0
for word in words2:
    # does not work:
    # print "words2[%(i)d] = \"%(words2[i])s\"" % vars()
    print "words2[%d]=%s" % (i,words2[i])
    i = i + 1

line = "white space   of varying    length"
print re.split(r"\s+", line)
print re.split(' ', line)



#----------------------------------------------------------------------------

line = "here is a line with double as a word, and again: double"
regex = re.search("double", line)
if regex: print "Yes, matched regex=", regex


# substitute Hello by Hi everywhere in a file:
filename = "hw.py"
file = open(filename, 'r')
lines = file.readlines()
file.close()
line = string.join(lines, "")   # join all lines into one string
re.sub('Hello','Hi',line)
print 'regex sub:\n',line

import re
print "\n\nTesting dictionaries:\n"
cmlargs = {'-tstop': '-6.0', '-c_in_H': '1.2'}   # default values
myargs  = ['-myopt', '9.9', '-tstop', '6.1', '-c_in_H', '9.8' ]
print "len(myargs)=",len(myargs)
arg_counter = 0
while arg_counter < len(myargs):
    option = myargs[arg_counter] 
    if option in cmlargs: 
	# next command-line argument is the value:
        arg_counter += 1
	value = myargs[arg_counter] 
        cmlargs[option] = value
        print "dictionary: cmlargs, key=", option," value=",value
    else:
        print "The option -%s is not registered" % option
        # don't stop this commontasks.py test... sys.exit(1)
    arg_counter += 1

# print the hash structure, key by key:
for item in cmlargs.keys():
    print "cmlargs['%s']=%s" % (item,cmlargs[item])
print "\n\n\n"

#----------------------------------------------------------------------
b = "1.2"           # b is a string
try: a = 0.5 * b    # b is NOT converted to a real number
except: print 'Python lacks auto convert of strings-numbers'

b = "1.2"           # b is a string
a = 0.5 * float(b)  # b is converted to a real number
print "a = ",a

myfile='hw.py'
import os.path
if os.path.isfile(myfile):
     print myfile, "is a plain file"
if os.path.isdir(myfile):
     print myfile, "is a directory"

import stat
myfile_stat = os.stat(myfile)
for entry in myfile_stat: print "stat: ",entry
filesize = myfile_stat[stat.ST_SIZE]
last_access = myfile_stat[stat.ST_ATIME]
mode = myfile_stat[stat.ST_MODE]
if stat.S_ISREG(mode):
    print "%(myfile)s is a regular file with %(filesize)d bytes " \
          "and last accessed %(last_access)s" % vars()


from scitools.misc import find
size_limit = 200
def checksize2(filepath, bigfiles):
    size = os.path.getsize(filepath)
    if size > size_limit:
        bigfiles.append('%.2fMb %s' % (size/1000000.0,filepath))

bigfiles = []
root = os.path.join(os.environ['scripting'],'src', 'py', 'regex')
find(checksize2, root, bigfiles)
for fileinfo in bigfiles:
    print fileinfo

import fnmatch
def checksize3(filepath, arg):
    treat_file = False
    ext = os.path.splitext(filepath)[1]
    import fnmatch  # Unix shell-style wildcard matching
    for s in arg['extensions']:
        if fnmatch.fnmatch(ext, s):
            treat_file = True
    size = os.path.getsize(filepath)
    if treat_file and size > arg['size_limit']:
        size = '%.2fMb' % (size/1000000.0)  # pretty print
        arg['filelist'].append({'size': size, 'name': filepath})

bigfiles = {'filelist': [],
            'extensions': ('.ps', '.tiff', '.bmp'),
            'size_limit': size_limit,
            }
find(checksize3, root, bigfiles)
# sort files according to size
def filesort(a, b):
    return cmp(float(a['size'][:-2]), float(b['size'][:-2]))
bigfiles['filelist'].sort(filesort)
bigfiles['filelist'].reverse()  # decreasing size
for fileinfo in bigfiles['filelist']:
    print fileinfo['name'], fileinfo['size']


def checksize1(arg, dirname, files):
    for file in files:
        filename = os.path.join(dirname, file)
        if os.path.isfile(filename):
            size = os.path.getsize(filename)
            if size > size_limit:
                size_in_Mb = size/1000000.0
                arg.append((size_in_Mb, filename))

print "\n\nversion 3 of tree traversal: root=",root
bigfiles = []
os.path.walk(root, checksize1, bigfiles)
for size, name in bigfiles:
    print name, 'is', size, 'Mb'

directory = "mynewdir"
print "creating", directory
os.mkdir(directory) # or os.mkdir(directory,'5557')
here = os.getcwd()
os.chdir(directory)
os.chdir(os.environ['HOME'])
os.chdir(here)
import shutil
print "removing", directory
shutil.rmtree(directory)


#-----------------------------------------------------------------------

def dump1(text, var):
    print text, var

v1 = ["listitem1","listitem2"]
dump1("leading text",v1)
dump1(var=v1,text="another leading text")

def dump2(text="No leading text", var=os.environ['HOME']):
    print text, var

# does the default value work?
dump2(var=v1)
dump2()

# functions must be defined before they can be used

def statistics(*args):
    """
    Compute the average, minimum and maximum of a list of numbers.
    Input: a variable no of arguments (numbers).
    Output: tuple (average, min, max).
    """
    # this function takes variable argument lists
    avg = 0;  n = 0 # avg and n are local variables
    for term in args:
        n = n + 1; avg = avg + term
    avg = avg / float(n)
    min = args[0]; max = args[0]
    for term in args:
        if term < min: min = term
        if term > max: max = term
    return [avg, min, max]

v1 = 1.1; v2 = 5.8; v3 = 9; b = 1;


[avg, min, max] = statistics(v1, v2, v3, b)
print "\bstatistics: avg=$avg, min=$min, max=$max\n"

# this doesn't work, classes would work, but not numbers and strings

def swap(a, b):
    tmp = a
    a = b
    b = tmp

v1 = 1.3; v2 = "some text"

print "before swap: v1=",v1," v2=",v2
swap(v1, v2) # swap the values of $v1 and $v2
print "after  swap: v1=",v1," v2=",v2

#----------------------------------------------------------------------------

import glob
filelist = glob.glob('*.ps') + glob.glob('*.gif') + glob.glob('*.py')
print "files of *.ps *.gif and *.py type:"
for file in filelist: print file

os.mkdir('tmp')
filelist = glob.glob('*.py')
for file in filelist: shutil.copy(file,'tmp')
shutil.rmtree('tmp')

# redirect output from a command into a variable:
cmd = "perl -pe '' hw.py"
resfile = os.popen(cmd)
res = resfile.readlines()
print "Output of the command " + cmd + " was\n"
for line in res:
    print line,   # the comma prevents extra newline
import commands
from scitools.misc import system
failure, res = system(cmd)
print res
    
    

#----------------------------------------------------------------------------

name = "/usr/home/hpl/scripting/perl/intro/hw.pl"
[head,tail] = os.path.split(name)
print "file=",name
print "head=",head
print "tail=",tail
basename = os.path.basename(name)
dirname  = os.path.dirname(name)
print "dirname =",dirname
print "basename=",basename

# make path:
directory = os.path.join('tmp','some','tmp','tmp')
os.makedirs(directory)
if os.path.isdir(directory):
    print "\nYes! created", directory
shutil.rmtree('tmp')  # not rmtree(directory); need to remove the root

# --------------------------------------------------------------------

print '\n\nProgramming with classes:'

class MyBase:
    def __init__(self,i,j):  # constructor
        self.i = i; self.j = j
    def write(self):
        print "MyBase: i=",self.i,"j=",self.j

class MySub(MyBase):
    def __init__(self,i,j,k):  # constructor
        MyBase.__init__(self,i,j)
        self.k = k
    def write(self):
        print "MySub: i=",self.i,"j=",self.j,"k=",self.k

def write(var): var.write()

i1 = MyBase(5,7)
write(i1)
i2 = MySub(7,8,9)
write(i2)

if isinstance(i1, MyBase):
    print 'i1 is MyBase'
if isinstance(i2, MySub):
    print 'i2 is MySub'
if isinstance(i2, MyBase):
    print 'i2 is MyBase too'

print "i1.__dict__:", i1.__dict__
print "i2.__dict__:", i2.__dict__

print "Names:", i1.__class__.__name__, \
      i1.__class__.__name__+'.'+i1.write.__name__
print "dir(i1):", dir(i1)
print "dir(i2):", dir(i2)
i2.__dict__['q'] = 'some string'
print i2.q
print dir(i2)
print "\n\n"


print '\n\nmultiple inheritance:'
class A:
    def set(self, a):
        self.a = a;  print 'A.set'

class B:
    def set(self, b):
        self.b = b;  print 'B.set'

class C(A, B):
    def set(self, c):
        self.c = c;  print 'C.set'

    def somefunc(self, x, y):
        A.set(self, x)   # call base class method
        B.set(self, y)   # call base class method
        self.set(0)      # call C's set method

c = C()
c.somefunc(2,3)
print c.__dict__

# ---------------------------------------------------------------------------

list = ["t1", "t2", "t3"]
try:
    list[6]
except IndexError:
    print "list[6] raises an exception"
    print "Exception type=",sys.exc_type
    print "Exception value=",sys.exc_value

average_exc = "Empty argument list in average"

def average(*args):
    i=0; sum=0
    for term in args: sum = sum + term; i=i+1
    if i == 0:
        raise average_exc
    else:
        return sum/float(i)

try:
    print "average=",average(1,2,3)
    print "average=",average()
except average_exc:
    print average_exc
    print "Exception type=",sys.exc_type
    print "Exception value=",sys.exc_value

try: f = open('ppp','r')
except:
    print "Exception type=",sys.exc_type
    print "Exception value=",sys.exc_value


# sending two lists to a function:

def displaylist(list=[], help=[]):
    for i in range(len(list)):
        print "item %d: %-20s  description: %s" % \
              (i, item[i], help[i])

curvelist = ['curve1', 'curve2', 'curve3']
explanations = ['initial shape of u',
                'initial shape of H',
                'shape of u at time=2.5']

# send the two lists to displaylist:
displaylist(list=curvelist, help=explanations)

def displaylist2(list=[]):
    for (item,help) in list:
        print "%(item)-20s  description: %(help)s" % vars()

curvelist = [
   ('curve1', 'initial shape of u'),
   ('curve2', 'initial shape of H'),
   ('curve3', 'shape of u at time=2.5')]

# send the two lists to displaylist:
displaylist2(list=curvelist)

#-----------------------------------------------------------------
# string and number comparison:

def compare(a,b):
    if a < b:
        print a,"is less than",b,"(a is", type(a),"and b is",type(b),")"
    else:
        print a,"is greater than or equal to",b,"(a is", type(a),"and b is",type(b),")"

compare("1.2", 100)
compare(1.2, 100)

b =  1.2        # b is a number
b = "1.2"       # b is a string
#a = 0.5 * b    # b is NOT converted to a real number
a = 0.5 * float(b)    # this works

print 'testing string b < 100:',
if b < 100:
    print 'ok'
else:
    print 'error!'

print 'testing float(b) < 100:',
if float(b) < 100:
    print 'ok'
else:
    print 'error!'

#-----------------------------------------------------------------
# recursive dict pretty print:
INDENT=2
def pretty_dict_print(d, indent=INDENT):
    s = "{\n"
    keys = d.keys(); keys.sort()
    for key in keys:
        s += " "*indent + " %s : " % repr(key)
        if type(d[key]) == type({}):  # recursive call?
            s += pretty_dict_print(d[key], indent+4)
        else:
            s += "%s,\n"  % repr(d[key])
    s += " "*(indent+1) + "},\n"
    return s

g = {'a':4, 'b':{'c':'some', 'd':77}, 'e':{'i':4, 'f':{'h':2, 'g':1}}}
print pretty_dict_print(g)


#-----------------------------------------------------------------
def roots(a, b, c):
    """
    Return two roots of the quadratic algebraic equation
    ax^2 + bx + c = 0, where a, b, and c may be complex.
    """
    import cmath      # complex functions
    q = b*b - 4*a*c
    r1 = -(b - cmath.sqrt(q))/(2*a)
    r2 = -(b + cmath.sqrt(q))/(2*a)
    # r1 and r2 are complex because cmath.sqrt returns complex,
    # convert to real if possible:
    if r1.imag == 0.0:  r1 = r1.real
    if r2.imag == 0.0:  r2 = r2.real
    if r1 == r2:  r2 = None  # use r2=None to indicate double root
    return r1, r2 

def printroots(a, b, c):
    q1, q2 = roots(a, b, c)
    if q2 is None:
        print 'double %s root=%s' % (str(type(q1))[7:-2], q1)
    else:
        print '%s root=%s, %s root=%s' % \
              (str(type(q1))[7:-2], q1, str(type(q2))[7:-2], q2)

printroots(1, 2j, -1)  # roots: -1j (double)
printroots(1, -66+1j, 66j)  # roots: -1j and 66
printroots(1, 0, 8j)   # roots: +/- 2-2j
printroots(1, 0, -4)   # roots: +/- 4


#-----------------------------------------------------------------
# regex:
s1 = "  just a string   with leading and trailing white space   "
s2 = re.sub(r"^\s*", "", s1)
s3 = re.sub(r"\s*$", "", s2)
print "\nregex:\n\n", "[%s]\n" % s1, "[%s]\n" % s2, "[%s]\n" %  s3

fakefile = """
# a comment
import re
# another comment
if re.search(r"^#", sometext):
    print "match!"
# third comment
"""
# this one will not work:
comments = re.findall(r"^#.*$", fakefile)
print "comments=", comments
# need to apply re.M, but re.findall does not take flags,
# need to compile first:
c = re.compile(r"^#.*$", re.MULTILINE)
comments = c.findall(fakefile)
print "comments=", comments

#-----------------------------------------------------------------
from Numeric import *
"minimal demo of the Python interface to Gnuplot"
points1 = [[0,1.2], [1.1,5.2], [2,-0.3]]
# make a list of data points from two arrays with x and y values:
x = arange(0.0, 2.0, 0.1)
y = 3 - 2*x + 2*sin(4*x)
points2 = transpose(array([x,y]))
print "points2=",points2
import Gnuplot
g = Gnuplot.Gnuplot(persist=1)
# define two curves:
d1 = Gnuplot.Data(points1, with='lines', title='points1')
d2 = Gnuplot.Data(points2, with='linespoints', title='points2')
g.title('Simple Python-Gnuplot demo')
g.xlabel('t'); g.ylabel('max u')
# let the Gnuplot instance g plot the data objects d1 and d2:
g.plot(d1, d2)
# make a PostScript file containing the plot:
g.hardcopy(filename='tmp.ps', enhanced=1, mode='eps',
           color=0, fontname='Times-Roman', fontsize=28)
#g('pause 30')  # halt the screen plot for 30 seconds
# any valid Gnuplot command can be issued with g(command)

#-----------------------------------------------------------------
import urllib
# download file and store it locally as "downloadme.dat":
try:
    urllib.urlretrieve("http://www.ifi.uio.no/~hpl/downloadme.dat",
                       "downloadme.dat")
except:
    print "trying urlretrieve: No Internet connection"
# work with a URL as a file:
try:
    file = urllib.urlopen("http://www.ifi.uio.no/~hpl/downloadme.dat")
    lines = file.readlines()
    print "http://www.ifi.uio.no/~hpl/downloadme.dat\n","lines=",lines
    file.close()
except:
    print "trying urlopen: No Internet connection"
if os.path.isfile("downloadme.dat"): os.remove("downloadme.dat")

print "finished"
