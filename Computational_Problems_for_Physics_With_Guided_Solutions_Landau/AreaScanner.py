""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    
# AreaScanner: examples of use of formated output
# and  reading input from the keyboard. Also: Input/Output with files

import math
name= raw_input( 'Key in your name: ')     #raw_input is good for strings
print "Hi ",name
radius=input('Enter a radius: ')            # works with numerical values
print 'you entered radius= %8.5f'%radius               # formatted output
name= raw_input('Key in another name: ')     # raw_input good for strings
radius=input('Enter a radius: ')
print 'Enter new name and r in file Name.dat'
inpfile=open('Name.dat','r')                 # to read from file Name.dat
for line in inpfile:
    line=line.split()                         # splits components of line
    name=line[0]                                # first entry in the list
    print " Hi  %10s" %(name)                 # print Hi plus first entry
    r=float(line[1])                      # second entry convert to float
    print " r = %13.5f" %(r)           # converts x to float and print it
inpfile.close()
A=math.pi*r**2                        # use radius to find circles's area
print "Done, look in A.dat\n"
outfile=open('A.dat','w')
outfile.write( 'r=  %13.5f\n'%(r))
outfile.write('A =  %13.5f\n'%(A))
outfile.close()
print 'r = %13.5f'%(r)                                    # screen output
print 'A = %13.5f'%(A)             
print 'Now example of integer input '
age=int(input ('Now key in your age as an integer:  '))
print "age: %4d  years old,  you don't look it!\n"%(age)
print "Press a character to finish"
s=raw_input()

