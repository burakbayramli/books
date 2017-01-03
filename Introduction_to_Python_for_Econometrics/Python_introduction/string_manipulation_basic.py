from __future__ import division
from __future__ import print_function
import cStringIO
import re
import string
import sys
import textwrap
from pylab import *
from numpy import *
# End Imports


a = 'Python is'
b = 'a rewarding language.'
a + ' ' + b

a = 'Python is'
b = 'a rewarding language.'
' '.join([a,b])

a = 'Python is'
b = 'a rewarding language.'
''.join([a,' ',b])

words = ['Python','is','a','rewarding','language']
','.join(words)

a = 'Python is'
2*a

sio = cStringIO.StringIO()
for i in xrange(10000):
    sio.write('cStringIO is faster than +! ')
sio.seek(0)
sio.read()

s = 'Python is a rewarding language.'
s.split(' ')
s.split(' ',3)
s.rsplit(' ',3)

a = 'Python is'
b = 'a rewarding language.'
string.join((a,b))
string.join((a,b),':')

s = '    Python is a rewarding language.    '
s=s.strip()
s.strip('P')

s = 'Python is a rewarding language.'
s.find('i')
s.find('i',10,20)
s.rfind('i')

words = ['apple','banana','cherry','date']
words_with_a = []
for word in words:
   if word.find('a')>=0:
       words_with_a.append(word)
words_with_a

s = 'Python is a rewarding language.'
s.index('i')
try:
    s.index('q') # Error
except:
    print("Error detected in: s.index('q') # Error")
    error = sys.exc_info()
    print("Error type: " + str(error[0]) + " Error message: " + str(error[1]))

s = 'Python is a rewarding language.'
s.count('i')
s.count('i', 10, 20)

s = 'Python is a rewarding language.'
s.upper()
s.lower()

s = 'Python is a rewarding language.'
s.ljust(40)
s.rjust(40)
s.center(40)

s = 'Python is a rewarding language.'
s.replace('g','Q')
s.replace('is','Q')
s.replace('g','Q',2)

s = 'Python is a rewarding language. '
s = 10*s
textwrap.wrap(s)
textwrap.wrap(s,50)

pi
'{:12.5f}'.format(pi)
'{:12.5g}'.format(pi)
'{:12.5e}'.format(pi)

'{0:}'.format(pi)

'{0:}, {1:} and {2:} are all related to pi'.format(pi,pi+1,2*pi)
'{2:}, {0:} and {1:} reorder the output.'.format(pi,pi+1,2*pi)

'{0:0<20}'.format(pi) # Left, 0 padding, precion 20
'{0:0>20}'.format(pi) # Right, 0 padding, precion 20
'{0: >20}'.format(pi) # Right, space padding, precion 20
'{0:$^20}'.format(pi) # Center, dollar sign padding, precion 20

'{0:+}'.format(pi)
'{0:+}'.format(-1.0 * pi)
'{0:-}'.format(pi)
'{0: }'.format(pi)
'{0: }'.format(-1.0 * pi)

'{0:10}'.format(pi)
'{0:20}'.format(pi)
'{0:30}'.format(pi)

'{0:.10}'.format(1000000 * pi)
'{0:,.10}'.format(1000000 * pi)

'{0:.1}'.format(pi)
'{0:.2}'.format(pi)
'{0:.5}'.format(pi)

'{0:.5e}'.format(pi)
'{0:.5g}'.format(pi)
'{0:.5f}'.format(pi)
'{0:.5%}'.format(pi)
'{0:.5e}'.format(100000 * pi)
'{0:.5g}'.format(100000 * pi)
'{0:.5f}'.format(100000 * pi)

'{0: > 20.4f}, {1: > 20.4f}'.format(pi,-pi)
'{0: > 20,.2f}, {1: > 20,.2f}'.format(100000 * pi,-100000 * pi)

s = 'Python'
'{0:}'.format(s)
'{0: >20}'.format(s)
'{0:!>20}'.format(s)
'The formatted string is: {0:!<20}'.format(s)

price = 100.32
volume = 132000
'The price yesterday was {:} with volume {:}'.format(price,volume)
'The price yesterday was {0:} and the volume was {1:}'.format(price,volume)
'The price yesterday was {1:} and the volume was {0:}'.format(volume,price)
'The price yesterday was {price:} and the volume was {volume:}'.format(price=price,volume=volume)

price = 100.32
volume = 132000
'The price yesterday was %0.2f with volume %d' % (price, volume)
'The price yesterday was %(price)0.2f with volume %(volume)d' \
    % {'price': price, 'volume': volume}
'The price yesterday was %+0.3f and the volume was %010d' % (price, volume)

s = 'Find all numbers in this string: 32.43, 1234.98, and 123.8.'
re.findall('[\s][0-9]+\.\d*',s)
matches = re.finditer('[\s][0-9]+\.\d*',s)
for m in matches:
    print(s[m.span()[0]:m.span()[1]])

s = 'Find all numbers in this string: 32.43, 1234.98, and 123.8.'
re.sub('[\s][0-9]+\.\d*',' NUMBER',s)
def reverse(m):
    """Reverse the string in the MatchObject group"""
    s = m.group()
    s = s.rstrip()
    return ' ' + s[::-1]
re.sub('[\s][0-9]+\.\d*',reverse,s)

s = 'Find all numbers in this string: 32.43, 1234.98, and 123.8.'
numbers = re.compile('[\s][0-9]+\.\d*')
numbers.findall(s)

S = ['1234','1234.567','a','1234.a34','1.0','a123']
for s in S:
    try:
        # If integer, use int
        int(s)
        print(s, 'is an integer.')
    except:
        try:
            # If not integer, may be float
            float(s)
            print(s, 'is a float.')
        except:
            print('Unable to convert', s)

