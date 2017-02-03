Cdegrees = [-20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40]
index = 0
print '    C    F'
while index < len(Cdegrees):
    C = Cdegrees[index]
    F = (9.0/5)*C + 32
    print '%5d %5.1f' % (C, F)
    index += 1

Cdegrees = []
for C in range(-20, 45, 5):
    Cdegrees.append(C)
print Cdegrees

Cdegrees = []
for i in range(0, 21):
     C = -10 + i*2.5
     Cdegrees.append(C)
print Cdegrees

Cdegrees = []
n = 21
C_min = -10
C_max = 40
dC = (C_max - C_min)/float(n-1)  # increment in C
for i in range(0, n):
     C = -10 + i*dC
     Cdegrees.append(C)

Fdegrees = []
for C in Cdegrees:
    F = (9.0/5)*C + 32
    Fdegrees.append(F)

for i in range(len(Cdegrees)):
    C = Cdegrees[i]
    F = Fdegrees[i]
    print '%5.1f %5.1f' % (C, F)

n = 21
C_min = -10
C_max = 40
dC = (C_max - C_min)/float(n-1)  # increment in C

Cdegrees = [0]*n
for i in range(len(Cdegrees)):
     Cdegrees[i] = -10 + i*dC

Fdegrees = [0]*n
for i in range(len(Cdegrees)):
    Fdegrees[i] = (9.0/5)*Cdegrees[i] + 32

for i in range(len(Cdegrees)):
    print '%5.1f %5.1f' % (Cdegrees[i], Fdegrees[i])

for C, F in zip(Cdegrees, Fdegrees):
    print '%5d %5.1f' % (C, F)

for c in Cdegrees:
    c += 5
print Cdegrees

for i in range(len(Cdegrees)):
    Cdegrees[i] += 5
print Cdegrees

Cdegrees = range(-20, 41, 5)   # -20, -15, ..., 35, 40
Fdegrees = [(9.0/5)*C + 32 for C in Cdegrees]

table = [Cdegrees, Fdegrees]
import pprint
pprint.pprint(table)

table = []
for C, F in zip(Cdegrees, Fdegrees):
    table.append([C, F])
pprint.pprint(table)

table = [[C, F] for C, F in zip(Cdegrees, Fdegrees)]
pprint.pprint(table)

print table[4:]
print table[4:7][0:2]


