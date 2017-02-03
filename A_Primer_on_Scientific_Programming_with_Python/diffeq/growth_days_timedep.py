from scitools.std import *
x0 = 100                             # initial amount
import datetime
date1 = datetime.date(2008, 11, 3)
date2 = datetime.date(2009, 12, 31)
diff = date2 - date1
N = diff.days
index_set = range(N+1)

# Annual interest rate
p = linspace(4, 6, len(index_set))
r = p/360.0                          # daily interest rate
x = zeros(len(index_set))

# Solution
x[0] = x0
for n in index_set[1:]:
    x[n] = x[n-1] + (r[n-1]/100.0)*x[n-1]
print x
plot(index_set, x, 'ro', xlabel='days', ylabel='amount')
