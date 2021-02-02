```python
from sympy import *
init_printing(use_unicode= True)
from sympy.vector import *
```


```python
N = CoordSys3D('N')
```


```python
a = N.i -3*N.j+5*N.k
b = 4*N.i+8*N.k
c = -2*N.i+9*N.j+N.k
```


```python
#1
a &b
```




$$44$$




```python
b&(a)
```




$$44$$




```python
b&(c)
```




$$0$$




```python
#2
(-3*a+5*c)&b
```




$$-132$$




```python
15*(a-c)&b
```




$$660$$




```python
#3
a.magnitude()
```




$$\sqrt{35}$$




```python
(2*b).magnitude()
```




$$8 \sqrt{5}$$




```python
(-c).magnitude()
```




$$\sqrt{86}$$




```python
#4
(a+b).magnitude()
```




$$\sqrt{203}$$




```python
a.magnitude() +b.magnitude()
```




$$\sqrt{35} + 4 \sqrt{5}$$




```python
#5
(b+c).magnitude()
```




$$\sqrt{166}$$




```python
b.magnitude()+c.magnitude()
```




$$4 \sqrt{5} + \sqrt{86}$$




```python
#6
((a+c).magnitude())**2 + ((a-c).magnitude())**2 
-2*(a.magnitude()**2 +b.magnitude()**2)
```




$$-230$$




```python
#7
a&c
```




$$-24$$




```python
float(a.magnitude()*(c.magnitude()))
```




$$54.86346689738081$$




```python
#8
5*a & 13*b
```




$$2860$$




```python
65*a & b
```




$$2860$$




```python
#9
(15*a & b) +(15* a&c)
```




$$300$$




```python
15*a&(b+c)
```




$$300$$




```python
#10
a&(b-c)
```




$$68$$




```python
(a-b)&c
```




$$-24$$




```python
#17
p = 2*N.i+5*N.j
A = N.i+3*N.j+3*N.k
B = 3*N.i+ 5*N.j+5*N.k
AB = (B-A)
p& AB
```




$$14$$




```python
#18
p = -1*N.i-2*N.j+4*N.k
A = 0*N.i
B = 6*N.i+ 7*N.j+5*N.k
AB = (B-A)
p& AB
```




$$0$$




```python
#19
p = 3*N.k+4*N.j
A = 4*N.i+5*N.j-N.k
B = N.i+ 3*N.j
AB = (B-A)
p& AB
```




$$-5$$




```python
#20
p = 6*N.i-3*N.j-3*N.k
A = N.i+5*N.j+2*N.k
B = 3*N.i+ 4*N.j+1*N.k
AB = (B-A)
p& AB
```




$$18$$




```python

```


```python
def angle(a,b):
    theta = (a&b)/(a.magnitude()*(b.magnitude()))
    theta = acos(theta)
    return theta
```


```python
#22
a=N.i+N.j
b= 3*N.i+2*N.j+N.k
c=N.i+2*N.k
```


```python
float(angle(a,b)*180/pi)
```




$$19.106605350869096$$




```python
#23
float(angle(b,c)*180/pi)
```




$$53.300774799510116$$




```python
#24
float(angle(a+c,b+c)*180/pi)
```




$$7.955800083158086$$




```python

```


```python

```
