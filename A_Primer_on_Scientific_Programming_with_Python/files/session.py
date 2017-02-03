infile = open('deg1.dat', 'r')
filestr = infile.read()
filestr
words = filestr.split()
words
numbers = [float(w) for w in words]
mean = sum(numbers)/len(numbers)
mean

temps = {'Oslo': 13, 'London': 15.4, 'Paris': 17.5}
temps['Madrid'] = 26.0
for city in temps:
    print 'The temperature in %s is %g' % (city, temps[city])

if 'Berlin' in temps:
    print 'Berlin:', temps['Berlin']
else:
    print 'No temperature data for Berlin'

temps.keys()
temps.values()

'Oslo' in temps

for city in sorted(temps.keys()):
    print city

del temps['Oslo']
temps
len(temps)   # no of key-value pairs in dictionary

d = {'key1': {'key1': 2, 'key2': 3}, 'key2': 7}
type(d['key1'])
d['key1']
d['key1']['key1']
d['key1']['key2']
d['key2']['key1']  # nonsense, no dict in d['key2']
type(d['key2'])



s = 'Berlin: 18.4 C at 4 pm'
s[8:]     # from index 8 to the end of the string
s[8:12]   # index 8, 9, 10 and 11 (not 12!)
s[8:-1]   # from index 8 to the next last character
s[8:-8]

s.find('Berlin')  # where does 'Berlin' start?
s.find('pm')
s.find('Oslo')    # not found...
'Berlin' in s
'Oslo' in s
if 'C' in s:
    print 'C found'
else:
    print 'no C'

s.replace(' ', '__')
s.replace('Berlin', 'Bonn')
s.replace(s[:s.find(':')], 'Bonn')

s.split()

s.split(':')
s.split(':')[1].split('C')[0]

'file1.dat, file2.dat, file3.dat'.split(', ')

t = '1st line\n2nd line\n3rd line'
print t
t.splitlines()

s.lower()
s.upper()

s[18]
s[18] = 5
s[:18] + '5' + s[19:]

'a word'.isdigit()
'214'.isdigit()

', '.join(['Newton', 'Secant', 'Bisection'])

s = '   text with leading/trailing whitespace   '
s.strip()
s.lstrip()
s.rstrip()
          
'sentence with no capitals'.capitalize()

'Heading'.center(40, '*')
' Heading '.center(40, '*')


