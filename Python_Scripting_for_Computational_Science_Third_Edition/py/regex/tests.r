./tests.verify: test performed on 2005.08.31 


#### Test: ./tests.verify running fdmgrid.py 
[('0', '11')]
[('0', '0', '', '', '10', '10', '', '')]

[('1', '21'), ('1', '101')]
[('0.1', '0.1', '.1', '', '1.1', '1.1', '.1', ''), ('0', '0', '', '', '2E+00', '2', '', 'E+00')]

[('1', '21'), ('1', '11'), ('-10', '15')]
[('0', '0', '', '', '1', '1', '', ''), ('0', '0', '', '', '2', '2', '', ''), ('-1', '1', '', '', '1.5', '1.5', '.5', '')]

work with groupindex:
[('0', '11')]
[('0', '10')]

[('1', '21'), ('1', '101')]
[('0.1', '1.1'), ('0', '2E+00')]

[('1', '21'), ('1', '11'), ('-10', '15')]
[('0', '1'), ('0', '2'), ('-1', '1.5')]

non-capturing groups:
[('0', '10')]

[('0.1', '1.1'), ('0', '2E+00')]

[('0', '1'), ('0', '2'), ('-1', '1.5')]


avoid so many parenthesis (just two groups now for each interval):
[('0', '11')]
[('0', '10')]

[('1', '21'), ('1', '101')]
[('0.1', '1.1'), ('0', '2E+00')]

[('1', '21'), ('1', '11'), ('-10', '15')]
[('0', '1'), ('0', '2'), ('-1', '1.5')]


simpler regular expressions:
\[([^,]*),([^\]]*)\] \[([^:,]*):([^\]]*)\]
[('0', '11')]
[('0', '10')]

[('1', '21'), ('1', '101')]
[('0.1', '1.1'), ('0', '2E+00')]

[('1', '21'), ('1', '11'), ('-10', '15')]
[('0', '1'), ('0', '2'), ('-1', '1.5')]


alternative; simpler regular expressions:
\[(.*?),(.*?)\] \[(.*?):(.*?)\]
[('0,10] indices=[0', '11')]
[('0', '10')]

[('0.1,1.1]x[0,2E+00] indices=[1', '21'), ('1', '101')]
[('0.1', '1.1'), ('0', '2E+00')]

[('0,1]x[0,2]x[-1,1.5] [1', '21'), ('1', '11'), ('-10', '15')]
[('0', '1'), ('0', '2'), ('-1', '1.5')]

CPU time of fdmgrid.py: 0.1 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running htmlsubst.py 
The <em>subst.py pattern replacement file1
file2 ...</em> script replaces a <em>pattern</em>,
specified as a regular expression, by a <em>
replacement</em> string in a series of files
<em>file1</em>, <em>file2</em>, and so forth.

The <em>subst.py pattern replacement file1
file2 ...</b> script replaces a <b>pattern</b>,
specified as a regular expression, by a <b>
replacement</b> string in a series of files
<b>file1</b>, <b>file2</em>, and so forth.

CPU time of htmlsubst.py: 0.1 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running intervalre.py 
[0,55] matches!  limits: 0 and 55
[ 0, 55  ] matches!  limits: 0 and 55
[-4, 55 ]  matches!  limits: -4 and 55
[r,s] does not match

[0,55] matches!  limits: 0 and 55
[ 0, 55  ] matches!  limits: 0 and 55
[-4, 55 ]  matches!  limits: -4 and 55
[r,s] does not match

regex for interval:
\[\s*(-?\d(\.\d+|)[Ee][+\-]\d\d?|-?(\d+\.\d*|\d*\.\d+)|-?\d+)\s*,\s*(-?\d(\.\d+|)[Ee][+\-]\d\d?|-?(\d+\.\d*|\d*\.\d+)|-?\d+)\s*\]
[2,9] matches!  groups= ('2', None, None, '9', None, None)
[-44 , 1.54E-03] matches!  groups= ('-44', None, None, '1.54E-03', '.54', None)
[3, 6.4] matches!  groups= ('3', None, None, '6.4', None, '6.4')
[-100,2.0e-1] matches!  groups= ('-100', None, None, '2.0e-1', '.0', None)
[3.58652e+05 , 6E+09] matches!  groups= ('3.58652e+05', '.58652', None, '6E+09', '', None)
[-.3, -0.9 ] matches!  groups= ('-.3', None, '.3', '-0.9', None, '0.9')

regex for interval (avoid nested OR expressions):
\[\s*(-?\d\.?\d*[Ee][+\-]\d+|-?\d*\.\d*|-?\d+)\s*,\s*(-?\d\.?\d*[Ee][+\-]\d+|-?\d*\.\d*|-?\d+)\s*\]
[2,9] matches!  groups= ('2', '9')
[-44 , 1.54E-03] matches!  groups= ('-44', '1.54E-03')
[3, 6.4] matches!  groups= ('3', '6.4')
[-100,2.0e-1] matches!  groups= ('-100', '2.0e-1')
[3.58652e+05 , 6E+09] matches!  groups= ('3.58652e+05', '6E+09')
[-.3, -0.9 ] matches!  groups= ('-.3', '-0.9')
[-87652, 1.9856e-004] matches!  groups= ('-87652', '1.9856e-004')

regex for interval with named groups:
\[\s*(?P<lower>-?\d\.?\d*[Ee][+\-]\d+|-?\d*\.\d*|-?\d+)\s*,\s*(?P<upper>-?\d\.?\d*[Ee][+\-]\d+|-?\d*\.\d*|-?\d+)\s*\]
[2,9] matches! limits: 2.0 9.0
   groups= ('2', '9')
[-44 , 1.54E-03] matches! limits: -44.0 0.00154
   groups= ('-44', '1.54E-03')
[3, 6.4] matches! limits: 3.0 6.4
   groups= ('3', '6.4')
[-100,2.0e-1] matches! limits: -100.0 0.2
   groups= ('-100', '2.0e-1')
[3.58652e+05 , 6E+09] matches! limits: 358652.0 6000000000.0
   groups= ('3.58652e+05', '6E+09')
[-.3, -0.9 ] matches! limits: -0.3 -0.9
   groups= ('-.3', '-0.9')
[-87652, 1.9856e-004] matches! limits: -87652.0 0.00019856
   groups= ('-87652', '1.9856e-004')

regex for interval:
\[\s*(-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*,\s*(-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*\]
[2,9] matches!  groups= ('2', '2', None, None, '9', '9', None, None)
[-44 , 1.54E-03] matches!  groups= ('-44', '44', None, None, '1.54E-03', '1.54', '.54', 'E-03')
[3, 6.4] matches!  groups= ('3', '3', None, None, '6.4', '6.4', '.4', None)
[-100,2.0e-1] matches!  groups= ('-100', '100', None, None, '2.0e-1', '2.0', '.0', 'e-1')
[3.58652e+05 , 6E+09] matches!  groups= ('3.58652e+05', '3.58652', '.58652', 'e+05', '6E+09', '6', None, 'E+09')
[-.3, -0.9 ] matches!  groups= ('-.3', '.3', None, None, '-0.9', '0.9', '.9', None)
[-87652, 1.9856e-004] matches!  groups= ('-87652', '87652', None, None, '1.9856e-004', '1.9856', '.9856', 'e-004')


regex for interval using named 1st and 4th groups:
\[\s*(?P<lower>-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*,\s*(?P<upper>-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*\]
[2,9] matches! limits: 2.0 9.0
   groups= ('2', '2', None, None, '9', '9', None, None)
[-44 , 1.54E-03] matches! limits: -44.0 0.00154
   groups= ('-44', '44', None, None, '1.54E-03', '1.54', '.54', 'E-03')
[3, 6.4] matches! limits: 3.0 6.4
   groups= ('3', '3', None, None, '6.4', '6.4', '.4', None)
[-100,2.0e-1] matches! limits: -100.0 0.2
   groups= ('-100', '100', None, None, '2.0e-1', '2.0', '.0', 'e-1')
[3.58652e+05 , 6E+09] matches! limits: 358652.0 6000000000.0
   groups= ('3.58652e+05', '3.58652', '.58652', 'e+05', '6E+09', '6', None, 'E+09')
[-.3, -0.9 ] matches! limits: -0.3 -0.9
   groups= ('-.3', '.3', None, None, '-0.9', '0.9', '.9', None)
[-87652, 1.9856e-004] matches! limits: -87652.0 0.00019856
   groups= ('-87652', '87652', None, None, '1.9856e-004', '1.9856', '.9856', 'e-004')

[2,9] matches \[(.*),(.*)\] group(1)= 2 group(2)= 9
[-44 , 1.54E-03] matches \[(.*),(.*)\] group(1)= -44  group(2)=  1.54E-03
[3, 6.4] matches \[(.*),(.*)\] group(1)= 3 group(2)=  6.4
[-100,2.0e-1] matches \[(.*),(.*)\] group(1)= -100 group(2)= 2.0e-1
[3.58652e+05 , 6E+09] matches \[(.*),(.*)\] group(1)= 3.58652e+05  group(2)=  6E+09
[-.3, -0.9 ] matches \[(.*),(.*)\] group(1)= -.3 group(2)=  -0.9 
[-87652, 1.9856e-004] matches \[(.*),(.*)\] group(1)= -87652 group(2)=  1.9856e-004

with re.findall and real regex: \s*(-?\d(\.\d+|)[Ee][+\-]\d\d?|-?(\d+\.\d*|\d*\.\d+)|-?\d+)\s*
(nested OR expressions and many groups)
findall applied to [2,9] gives [('2', '', ''), ('9', '', '')]
findall applied to [-44 , 1.54E-03] gives [('-44', '', ''), ('1.54E-03', '.54', '')]
findall applied to [3, 6.4] gives [('3', '', ''), ('6.4', '', '6.4')]
findall applied to [-100,2.0e-1] gives [('-100', '', ''), ('2.0e-1', '.0', '')]
findall applied to [3.58652e+05 , 6E+09] gives [('3.58652e+05', '.58652', ''), ('6E+09', '', '')]
findall applied to [-.3, -0.9 ] gives [('-.3', '', '.3'), ('-0.9', '', '0.9')]
findall applied to [-87652, 1.9856e-004] gives [('-87652', '', ''), ('1.9856e-00', '.9856', ''), ('4', '', '')]

with re.findall and real regex: \s*(?P<upper>-?\d\.?\d*[Ee][+\-]\d+|-?\d*\.\d*|-?\d+)\s*
(no nested OR expressions and just one group)
findall applied to [2,9] gives ['2', '9']
findall applied to [-44 , 1.54E-03] gives ['-44', '1.54E-03']
findall applied to [3, 6.4] gives ['3', '6.4']
findall applied to [-100,2.0e-1] gives ['-100', '2.0e-1']
findall applied to [3.58652e+05 , 6E+09] gives ['3.58652e+05', '6E+09']
findall applied to [-.3, -0.9 ] gives ['-.3', '-0.9']
findall applied to [-87652, 1.9856e-004] gives ['-87652', '1.9856e-004']


with re.findall and real regex: \s*(?P<number>-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*
(named outer (main) group, many groups)
findall applied to [2,9] gives 2 and 9
findall applied to [-44 , 1.54E-03] gives 44 and 1.54
findall applied to [3, 6.4] gives 3 and 6.4
findall applied to [-100,2.0e-1] gives 100 and 2.0
findall applied to [3.58652e+05 , 6E+09] gives 3.58652 and 6
findall applied to [-.3, -0.9 ] gives .3 and 0.9
findall applied to [-87652, 1.9856e-004] gives 87652 and 1.9856

[2,9] matches! group("lower")= 2 group('upper')= 9
[-44 , 1.54E-03] matches! group("lower")= -44 group('upper')= 1.54E-03
[3, 6.4] matches! group("lower")= 3 group('upper')= 6.4
[-100,2.0e-1] matches! group("lower")= -100 group('upper')= 2.0e-1
[3.58652e+05 , 6E+09] matches! group("lower")= 3.58652e+05 group('upper')= 6E+09
[-.3, -0.9 ] matches! group("lower")= -.3 group('upper')= -0.9
[-87652, 1.9856e-004] matches! group("lower")= -87652 group('upper')= 1.9856e-004
CPU time of intervalre.py: 0.2 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running introre.py 
t = [2.5, 4.25, 5.0, 6.3860000000000001, 8.0500000000000007]
iterations = [12, 6, 2, 6, 3]
eps = [1.38756e-05, 2.2243300000000001e-05, 3.7879600000000002e-05, 2.22433e-06, 0.00091111100000000004]
CPU time of introre.py: 0.1 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running realre.py 

error: wrong number 2 is extracted from 'some text, a=2.54E-05, inside a string' 
all groups: ('2', None, None)
error: wrong number 2.54 is extracted from 'some text, a=2.54E-05, inside a string' 
all groups: ('2.54', '2.54', None)

correct number 2.54E-05 is extracted from 'some text, a=2.54E-05, inside a string' 
all groups: ('2.54', '.54', 'E-05')
CPU time of realre.py: 0.1 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running swap1.py 
Treating .test1.c
CPU time of swap1.py: 0.1 seconds on hplx30 i686, Linux



----- appending file .test1.c.tmp ------
#include <something.h>
void superLibFunc(float x, char* method)
{ printf("method=%s with x=%g",method,x); }

void someFunc ()
{
  /* calling up the super superLibFunc function */
  superLibFunc(x, a);  superLibFunc(ppp, qqq);
  /* other calls of superLibFunc */
  superLibFunc(method2 , method1);
  superLibFunc(method2 , 3method /* illegal name! */) ;  
  superLibFunc(method_2, _method1) ;
  /* here is a more challenging call wrt regex construction: */
  superLibFunc(super_method4 /* a special method that
                                 deserves a two-line comment... */
               , method1 /* the first method we have */ ) ;
}


#### Test: ./tests.verify running swap2.py 
Treating .test1.c
CPU time of swap2.py: 0.1 seconds on hplx30 i686, Linux



----- appending file .test1.c.tmp ------
#include <something.h>
void superLibFunc(float x, char* method)
{ printf("method=%s with x=%g",method,x); }

void someFunc ()
{
  /* calling up the super superLibFunc function */
  superLibFunc(x, a);  superLibFunc(ppp, qqq);
  /* other calls of superLibFunc */
  superLibFunc(method2 , method1);
  superLibFunc(method2 , 3method /* illegal name! */) ;  
  superLibFunc(method_2, _method1) ;
  /* here is a more challenging call wrt regex construction: */
  superLibFunc(super_method4 /* a special method that
                                 deserves a two-line comment... */
               , method1 /* the first method we have */ ) ;
}


#### Test: ./tests.verify running swap3.py 
Treating .test1.c
CPU time of swap3.py: 0.1 seconds on hplx30 i686, Linux



----- appending file .test1.c.tmp ------
#include <something.h>
void superLibFunc(float x, char* method))
{ printf("method=%s with x=%g",method,x); }

void someFunc ()
{
  /* calling up the super superLibFunc function */
  superLibFunc(x,a));  superLibFunc(ppp,qqq));
  /* other calls of superLibFunc */
  superLibFunc(method2, method1 ));
  superLibFunc(method2, 3method /* illegal name! */ )) ;  
  superLibFunc(method_2,_method1)) ;
  /* here is a more challenging call wrt regex construction: */
  superLibFunc(super_method4 /* a special method that
                                 deserves a two-line comment... */ ,
            method1 /* the first method we have */
               )) ;
}

