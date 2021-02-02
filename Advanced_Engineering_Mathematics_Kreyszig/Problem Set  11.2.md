```python
%matplotlib inline
```


```python
#1
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- np.pi,np.pi,400)
y = np.exp(x)

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c7d62b0>]




    
![png](output_1_1.png)
    



```python
#1
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 10,10,400)
y = []
for i in x:
    if i < 0:
        y.append(np.exp(i))
    else:
        y.append(np.exp(-i))

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c768a90>]




    
![png](output_2_1.png)
    



```python
#1
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 50,50,4000)
y = []
for i in x:
    y.append((i**3)*np.cos(2*i))

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c6d8c50>]




    
![png](output_3_1.png)
    



```python
#1
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 10,10,400)
y = []
for i in x:
    y.append((i**2)*np.tan(np.pi*i))

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c6b64a8>]




    
![png](output_4_1.png)
    



```python
#1
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 10,10,400)
y = []
for i in x:
    y.append(np.sinh(i)-np.cos(i))

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c619470>]




    
![png](output_5_1.png)
    



```python
#2
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 10,10,400)
y = []
for i in x:
    y.append(np.sin(i)**2)

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c5f8ac8>]




    
![png](output_6_1.png)
    



```python
#2
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 5,5,400)
y = []
for i in x:
    y.append(np.sin(i**2))

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c5606d8>]




    
![png](output_7_1.png)
    



```python
#2
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 5,5,400)
y = []
for i in x:
    y.append(np.log(i))

y = np.array(y)
plt.plot(x,y,'-')
```

    /home/janani/anaconda3/lib/python3.6/site-packages/ipykernel_launcher.py:8: RuntimeWarning: invalid value encountered in log
      





    [<matplotlib.lines.Line2D at 0x7fd31c53fef0>]




    
![png](output_8_2.png)
    



```python
#2
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 5,5,400)
y = []
for i in x:
    y.append(i/(1+i**2))

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c4ac080>]




    
![png](output_9_1.png)
    



```python
#2
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 5,5,400)
y = []
for i in x:
    y.append(i/np.tan(i))

y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd31c47ada0>]




    
![png](output_10_1.png)
    


   #3.
   Even Functions is f(x) where f(-x)= f(x).
   
   So sum of even functions:
   
   g(-x)+ f(-x) = g(x)+f(x) .
   
   if both g(x) and f(x) are even function then their sum is also even
   
   So product of even functions:
   
   g(-x)* f(-x) = g(x)*f(x) .
   
   if both g(x) and f(x) are even function then their product is also even

#4. odd Functions is f(x) where f(-x)= -f(x)
   
   So sum of odd functions:
   
   g(-x)+ f(-x) = -g(x)-f(x) 
   
   if both g(x) and f(x) are even function then their sum is also odd
   
   So product of odd functions:
   
   g(-x)* f(-x) = g(x)*f(x) 
   
   if both g(x) and f(x) are even function then their product is even

6.let g(x) be odd function so g(-x) = -g(x)
let f(x) be even function so f(-x) = f(x)
do odd times even f(x).g(x) 
f(-x).g(-x)= - f(x).g(x)
so the product is also odd


```python
#8 its a even funtion 

from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols("x")

def even(k,l):
    a0 = integrate(x,(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i = 2*integrate(x*cos(j*pi*x/l), (x,0,l))/l
        expr += i*cos(j*pi*x/l)
        j+=1
    return expr

plot(even(40,1),(x,-2,2))
```


    
![png](output_14_0.png)
    





    <sympy.plotting.plot.Plot at 0x7f6ba9aab7b8>




```python
even(20,1)
```




$$- \frac{4}{\pi^{2}} \cos{\left (\pi x \right )} - \frac{4}{9 \pi^{2}} \cos{\left (3 \pi x \right )} - \frac{4}{25 \pi^{2}} \cos{\left (5 \pi x \right )} - \frac{4}{49 \pi^{2}} \cos{\left (7 \pi x \right )} - \frac{4}{81 \pi^{2}} \cos{\left (9 \pi x \right )} - \frac{4}{121 \pi^{2}} \cos{\left (11 \pi x \right )} - \frac{4}{169 \pi^{2}} \cos{\left (13 \pi x \right )} - \frac{4}{225 \pi^{2}} \cos{\left (15 \pi x \right )} - \frac{4}{289 \pi^{2}} \cos{\left (17 \pi x \right )} - \frac{4}{361 \pi^{2}} \cos{\left (19 \pi x \right )} + \frac{1}{2}$$




```python
#9 its a even function
x = symbols("x")

def odd(k,l):
    expr =0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i = 2*integrate(sin(j*pi*x/l), (x,0,l))/l
        expr += i*sin(j*pi*x/l)
        j+=1
    return expr

plt.show(plot(odd(40,2),(x,-4,4)))
```


    
![png](output_16_0.png)
    



```python
odd(40,2)
```




$$\frac{4}{\pi} \sin{\left (\frac{\pi x}{2} \right )} + \frac{4}{3 \pi} \sin{\left (\frac{3 \pi}{2} x \right )} + \frac{4}{5 \pi} \sin{\left (\frac{5 \pi}{2} x \right )} + \frac{4}{7 \pi} \sin{\left (\frac{7 \pi}{2} x \right )} + \frac{4}{9 \pi} \sin{\left (\frac{9 \pi}{2} x \right )} + \frac{4}{11 \pi} \sin{\left (\frac{11 \pi}{2} x \right )} + \frac{4}{13 \pi} \sin{\left (\frac{13 \pi}{2} x \right )} + \frac{4}{15 \pi} \sin{\left (\frac{15 \pi}{2} x \right )} + \frac{4}{17 \pi} \sin{\left (\frac{17 \pi}{2} x \right )} + \frac{4}{19 \pi} \sin{\left (\frac{19 \pi}{2} x \right )} + \frac{4}{21 \pi} \sin{\left (\frac{21 \pi}{2} x \right )} + \frac{4}{23 \pi} \sin{\left (\frac{23 \pi}{2} x \right )} + \frac{4}{25 \pi} \sin{\left (\frac{25 \pi}{2} x \right )} + \frac{4}{27 \pi} \sin{\left (\frac{27 \pi}{2} x \right )} + \frac{4}{29 \pi} \sin{\left (\frac{29 \pi}{2} x \right )} + \frac{4}{31 \pi} \sin{\left (\frac{31 \pi}{2} x \right )} + \frac{4}{33 \pi} \sin{\left (\frac{33 \pi}{2} x \right )} + \frac{4}{35 \pi} \sin{\left (\frac{35 \pi}{2} x \right )} + \frac{4}{37 \pi} \sin{\left (\frac{37 \pi}{2} x \right )} + \frac{4}{39 \pi} \sin{\left (\frac{39 \pi}{2} x \right )}$$




```python
#10 is odd function

def odd(k,l):
    expr =0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i = 2*integrate((4-x)*sin(j*pi*x/l), (x,0,l))/l
        expr += i*sin(j*pi*x/l)
        j+=1
    return expr

plt.show(plot(odd(40,4),(x,-8,8)))
```


    
![png](output_18_0.png)
    



```python
odd(40,4)
```




$$\frac{8}{\pi} \sin{\left (\frac{\pi x}{4} \right )} + \frac{4}{\pi} \sin{\left (\frac{\pi x}{2} \right )} + \frac{8}{3 \pi} \sin{\left (\frac{3 \pi}{4} x \right )} + \frac{2}{\pi} \sin{\left (\pi x \right )} + \frac{8}{5 \pi} \sin{\left (\frac{5 \pi}{4} x \right )} + \frac{4}{3 \pi} \sin{\left (\frac{3 \pi}{2} x \right )} + \frac{8}{7 \pi} \sin{\left (\frac{7 \pi}{4} x \right )} + \frac{1}{\pi} \sin{\left (2 \pi x \right )} + \frac{8}{9 \pi} \sin{\left (\frac{9 \pi}{4} x \right )} + \frac{4}{5 \pi} \sin{\left (\frac{5 \pi}{2} x \right )} + \frac{8}{11 \pi} \sin{\left (\frac{11 \pi}{4} x \right )} + \frac{2}{3 \pi} \sin{\left (3 \pi x \right )} + \frac{8}{13 \pi} \sin{\left (\frac{13 \pi}{4} x \right )} + \frac{4}{7 \pi} \sin{\left (\frac{7 \pi}{2} x \right )} + \frac{8}{15 \pi} \sin{\left (\frac{15 \pi}{4} x \right )} + \frac{1}{2 \pi} \sin{\left (4 \pi x \right )} + \frac{8}{17 \pi} \sin{\left (\frac{17 \pi}{4} x \right )} + \frac{4}{9 \pi} \sin{\left (\frac{9 \pi}{2} x \right )} + \frac{8}{19 \pi} \sin{\left (\frac{19 \pi}{4} x \right )} + \frac{2}{5 \pi} \sin{\left (5 \pi x \right )} + \frac{8}{21 \pi} \sin{\left (\frac{21 \pi}{4} x \right )} + \frac{4}{11 \pi} \sin{\left (\frac{11 \pi}{2} x \right )} + \frac{8}{23 \pi} \sin{\left (\frac{23 \pi}{4} x \right )} + \frac{1}{3 \pi} \sin{\left (6 \pi x \right )} + \frac{8}{25 \pi} \sin{\left (\frac{25 \pi}{4} x \right )} + \frac{4}{13 \pi} \sin{\left (\frac{13 \pi}{2} x \right )} + \frac{8}{27 \pi} \sin{\left (\frac{27 \pi}{4} x \right )} + \frac{2}{7 \pi} \sin{\left (7 \pi x \right )} + \frac{8}{29 \pi} \sin{\left (\frac{29 \pi}{4} x \right )} + \frac{4}{15 \pi} \sin{\left (\frac{15 \pi}{2} x \right )} + \frac{8}{31 \pi} \sin{\left (\frac{31 \pi}{4} x \right )} + \frac{1}{4 \pi} \sin{\left (8 \pi x \right )} + \frac{8}{33 \pi} \sin{\left (\frac{33 \pi}{4} x \right )} + \frac{4}{17 \pi} \sin{\left (\frac{17 \pi}{2} x \right )} + \frac{8}{35 \pi} \sin{\left (\frac{35 \pi}{4} x \right )} + \frac{2}{9 \pi} \sin{\left (9 \pi x \right )} + \frac{8}{37 \pi} \sin{\left (\frac{37 \pi}{4} x \right )} + \frac{4}{19 \pi} \sin{\left (\frac{19 \pi}{2} x \right )} + \frac{8}{39 \pi} \sin{\left (\frac{39 \pi}{4} x \right )}$$




```python
#11 is an even function

def even(k,l):
    a0 = integrate(x*x,(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i = 2*integrate(x*x*cos(j*pi*x/l), (x,0,l))/l
        expr += i*cos(j*pi*x/l)
        j+=1
    return expr

plt.show(plot(even(20,1),(x,-2,2)))
```


    
![png](output_20_0.png)
    



```python
even(20,1)
```




$$- \frac{4}{\pi^{2}} \cos{\left (\pi x \right )} + \frac{1}{\pi^{2}} \cos{\left (2 \pi x \right )} - \frac{4}{9 \pi^{2}} \cos{\left (3 \pi x \right )} + \frac{1}{4 \pi^{2}} \cos{\left (4 \pi x \right )} - \frac{4}{25 \pi^{2}} \cos{\left (5 \pi x \right )} + \frac{1}{9 \pi^{2}} \cos{\left (6 \pi x \right )} - \frac{4}{49 \pi^{2}} \cos{\left (7 \pi x \right )} + \frac{1}{16 \pi^{2}} \cos{\left (8 \pi x \right )} - \frac{4}{81 \pi^{2}} \cos{\left (9 \pi x \right )} + \frac{1}{25 \pi^{2}} \cos{\left (10 \pi x \right )} - \frac{4}{121 \pi^{2}} \cos{\left (11 \pi x \right )} + \frac{1}{36 \pi^{2}} \cos{\left (12 \pi x \right )} - \frac{4}{169 \pi^{2}} \cos{\left (13 \pi x \right )} + \frac{1}{49 \pi^{2}} \cos{\left (14 \pi x \right )} - \frac{4}{225 \pi^{2}} \cos{\left (15 \pi x \right )} + \frac{1}{64 \pi^{2}} \cos{\left (16 \pi x \right )} - \frac{4}{289 \pi^{2}} \cos{\left (17 \pi x \right )} + \frac{1}{81 \pi^{2}} \cos{\left (18 \pi x \right )} - \frac{4}{361 \pi^{2}} \cos{\left (19 \pi x \right )} + \frac{1}{3}$$




```python
#12 
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- 2,2,400)
y = []
for i in x:
    y.append(1-i**2/4)    
y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fd30ea2df98>]




    
![png](output_22_1.png)
    



```python
# so 12 is even function
x = symbols("x")
def even(k,l):
    
    a0 = integrate(1-x**2/4,(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i = 2*integrate((1-x**2/4)*cos(j*pi*x/l), (x,0,l))/l
        expr += i*cos(j*pi*x/l)
        j+=1
    return expr

plt.show(plot(even(20,2),(x,-4,4)))

```


    
![png](output_23_0.png)
    



```python
even(20,2)
```




$$\frac{4}{\pi^{2}} \cos{\left (\frac{\pi x}{2} \right )} - \frac{1}{\pi^{2}} \cos{\left (\pi x \right )} + \frac{4}{9 \pi^{2}} \cos{\left (\frac{3 \pi}{2} x \right )} - \frac{1}{4 \pi^{2}} \cos{\left (2 \pi x \right )} + \frac{4}{25 \pi^{2}} \cos{\left (\frac{5 \pi}{2} x \right )} - \frac{1}{9 \pi^{2}} \cos{\left (3 \pi x \right )} + \frac{4}{49 \pi^{2}} \cos{\left (\frac{7 \pi}{2} x \right )} - \frac{1}{16 \pi^{2}} \cos{\left (4 \pi x \right )} + \frac{4}{81 \pi^{2}} \cos{\left (\frac{9 \pi}{2} x \right )} - \frac{1}{25 \pi^{2}} \cos{\left (5 \pi x \right )} + \frac{4}{121 \pi^{2}} \cos{\left (\frac{11 \pi}{2} x \right )} - \frac{1}{36 \pi^{2}} \cos{\left (6 \pi x \right )} + \frac{4}{169 \pi^{2}} \cos{\left (\frac{13 \pi}{2} x \right )} - \frac{1}{49 \pi^{2}} \cos{\left (7 \pi x \right )} + \frac{4}{225 \pi^{2}} \cos{\left (\frac{15 \pi}{2} x \right )} - \frac{1}{64 \pi^{2}} \cos{\left (8 \pi x \right )} + \frac{4}{289 \pi^{2}} \cos{\left (\frac{17 \pi}{2} x \right )} - \frac{1}{81 \pi^{2}} \cos{\left (9 \pi x \right )} + \frac{4}{361 \pi^{2}} \cos{\left (\frac{19 \pi}{2} x \right )} + \frac{2}{3}$$




```python
#13 is neither even nor odd


x = symbols("x")
def fourier(k,l):
    a0 = integrate(x,(x,0,l))/(2*l)
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in syma:
        i = integrate(x*cos(j*pi*x/l), (x,0,l))/l
        expr += i*cos(j*pi*x/l)
        j+=1
    j = 1
    for i in symb:
        i = integrate(x*sin(j*pi*x/l), (x,0,l))/l
        expr += i*sin(j*pi*x/l)
        j+= 1
    return expr

plt.show(plot(fourier(20,.5),(x,-4,4)))
```


    
![png](output_25_0.png)
    



```python
fourier(20,.5)
```




$$\frac{0.5}{\pi} \sin{\left (2.0 \pi x \right )} - \frac{0.25}{\pi} \sin{\left (4.0 \pi x \right )} + \frac{0.166666666666667}{\pi} \sin{\left (6.0 \pi x \right )} - \frac{0.125}{\pi} \sin{\left (8.0 \pi x \right )} + \frac{0.1}{\pi} \sin{\left (10.0 \pi x \right )} - \frac{0.0833333333333333}{\pi} \sin{\left (12.0 \pi x \right )} + \frac{0.0714285714285714}{\pi} \sin{\left (14.0 \pi x \right )} - \frac{0.0625}{\pi} \sin{\left (16.0 \pi x \right )} + \frac{0.0555555555555556}{\pi} \sin{\left (18.0 \pi x \right )} - \frac{0.05}{\pi} \sin{\left (20.0 \pi x \right )} + \frac{0.0454545454545455}{\pi} \sin{\left (22.0 \pi x \right )} - \frac{0.0416666666666667}{\pi} \sin{\left (24.0 \pi x \right )} + \frac{0.0384615384615385}{\pi} \sin{\left (26.0 \pi x \right )} - \frac{0.0357142857142857}{\pi} \sin{\left (28.0 \pi x \right )} + \frac{0.0333333333333333}{\pi} \sin{\left (30.0 \pi x \right )} - \frac{0.03125}{\pi} \sin{\left (32.0 \pi x \right )} + \frac{0.0294117647058824}{\pi} \sin{\left (34.0 \pi x \right )} - \frac{0.0277777777777778}{\pi} \sin{\left (36.0 \pi x \right )} + \frac{0.0263157894736842}{\pi} \sin{\left (38.0 \pi x \right )} - \frac{1.0}{\pi^{2}} \cos{\left (2.0 \pi x \right )} - \frac{0.111111111111111}{\pi^{2}} \cos{\left (6.0 \pi x \right )} - \frac{0.04}{\pi^{2}} \cos{\left (10.0 \pi x \right )} - \frac{0.0204081632653061}{\pi^{2}} \cos{\left (14.0 \pi x \right )} - \frac{0.0123456790123457}{\pi^{2}} \cos{\left (18.0 \pi x \right )} - \frac{0.00826446280991736}{\pi^{2}} \cos{\left (22.0 \pi x \right )} - \frac{0.00591715976331361}{\pi^{2}} \cos{\left (26.0 \pi x \right )} - \frac{0.00444444444444444}{\pi^{2}} \cos{\left (30.0 \pi x \right )} - \frac{0.00346020761245675}{\pi^{2}} \cos{\left (34.0 \pi x \right )} - \frac{0.00277008310249307}{\pi^{2}} \cos{\left (38.0 \pi x \right )} + 0.125$$




```python
#14 is an even function


x = symbols("x")
def fourier(k,l):
    a0 = integrate(cos(pi*x),(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    
    j=1
    for i in syma:
        i = 2*integrate(cos(pi*x)*cos(j*pi*x/l), (x,0,l))/l
        expr += i*cos(j*pi*x/l)
        j+=1
    
    return expr

plt.show(plot(fourier(20,.5),(x,-2,2)))
```


    
![png](output_27_0.png)
    



```python
fourier(20,.5)
```




$$\frac{1.33333333333333}{\pi} \cos{\left (2.0 \pi x \right )} - \frac{0.266666666666667}{\pi} \cos{\left (4.0 \pi x \right )} + \frac{0.114285714285714}{\pi} \cos{\left (6.0 \pi x \right )} - \frac{0.0634920634920635}{\pi} \cos{\left (8.0 \pi x \right )} + \frac{0.0404040404040404}{\pi} \cos{\left (10.0 \pi x \right )} - \frac{0.027972027972028}{\pi} \cos{\left (12.0 \pi x \right )} + \frac{0.0205128205128205}{\pi} \cos{\left (14.0 \pi x \right )} - \frac{0.0156862745098039}{\pi} \cos{\left (16.0 \pi x \right )} + \frac{0.0123839009287926}{\pi} \cos{\left (18.0 \pi x \right )} - \frac{0.0100250626566416}{\pi} \cos{\left (20.0 \pi x \right )} + \frac{0.0082815734989648}{\pi} \cos{\left (22.0 \pi x \right )} - \frac{0.00695652173913044}{\pi} \cos{\left (24.0 \pi x \right )} + \frac{0.00592592592592593}{\pi} \cos{\left (26.0 \pi x \right )} - \frac{0.00510855683269476}{\pi} \cos{\left (28.0 \pi x \right )} + \frac{0.00444938820912125}{\pi} \cos{\left (30.0 \pi x \right )} - \frac{0.00391006842619746}{\pi} \cos{\left (32.0 \pi x \right )} + \frac{0.00346320346320346}{\pi} \cos{\left (34.0 \pi x \right )} - \frac{0.00308880308880309}{\pi} \cos{\left (36.0 \pi x \right )} + \frac{0.00277200277200277}{\pi} \cos{\left (38.0 \pi x \right )} + \frac{2.0}{\pi}$$




```python
#15 is an odd function 

x = symbols("x")
def fourier(k,l):
    #a0 = integrate(cos(pi*x),(x,0,l))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb  = symbols(strb)    
    j=1
    for i in symb:
        i = 2*(integrate(x*sin(j*pi*x/l),(x,0,pi/2))+integrate((pi-x)*sin(j*pi*x/l),(x,pi/2,pi)))/l
        expr += i*sin(j*pi*x/l)
        j+=1
    
    return expr

plt.show(plot(fourier(20,pi),(x,-6,6)))
```


    
![png](output_29_0.png)
    



```python
fourier(20,pi)
```




$$\frac{4}{\pi} \sin{\left (x \right )} - \frac{4}{9 \pi} \sin{\left (3 x \right )} + \frac{4}{25 \pi} \sin{\left (5 x \right )} - \frac{4}{49 \pi} \sin{\left (7 x \right )} + \frac{4}{81 \pi} \sin{\left (9 x \right )} - \frac{4}{121 \pi} \sin{\left (11 x \right )} + \frac{4}{169 \pi} \sin{\left (13 x \right )} - \frac{4}{225 \pi} \sin{\left (15 x \right )} + \frac{4}{289 \pi} \sin{\left (17 x \right )} - \frac{4}{361 \pi} \sin{\left (19 x \right )}$$




```python

```


```python
#16 is an odd function 

x = symbols("x")
def fourier(k,l):
    #a0 = integrate(cos(pi*x),(x,0,l))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb  = symbols(strb)    
    j=1
    for i in symb:
        i = 2*integrate((x**2)*sin(j*pi*x/l),(x,0,1))/l
        expr += i*sin(j*pi*x/l)
        j+=1
    
    return expr

plt.show(plot(fourier(20,1),(x,-2,2)))
```


    
![png](output_32_0.png)
    



```python
fourier(20,1)
```




$$\left(- \frac{8}{\pi^{3}} + \frac{2}{\pi}\right) \sin{\left (\pi x \right )} - \frac{1}{\pi} \sin{\left (2 \pi x \right )} + \left(- \frac{8}{27 \pi^{3}} + \frac{2}{3 \pi}\right) \sin{\left (3 \pi x \right )} - \frac{1}{2 \pi} \sin{\left (4 \pi x \right )} + \left(- \frac{8}{125 \pi^{3}} + \frac{2}{5 \pi}\right) \sin{\left (5 \pi x \right )} - \frac{1}{3 \pi} \sin{\left (6 \pi x \right )} + \left(- \frac{8}{343 \pi^{3}} + \frac{2}{7 \pi}\right) \sin{\left (7 \pi x \right )} - \frac{1}{4 \pi} \sin{\left (8 \pi x \right )} + \left(- \frac{8}{729 \pi^{3}} + \frac{2}{9 \pi}\right) \sin{\left (9 \pi x \right )} - \frac{1}{5 \pi} \sin{\left (10 \pi x \right )} + \left(- \frac{8}{1331 \pi^{3}} + \frac{2}{11 \pi}\right) \sin{\left (11 \pi x \right )} - \frac{1}{6 \pi} \sin{\left (12 \pi x \right )} + \left(- \frac{8}{2197 \pi^{3}} + \frac{2}{13 \pi}\right) \sin{\left (13 \pi x \right )} - \frac{1}{7 \pi} \sin{\left (14 \pi x \right )} + \left(- \frac{8}{3375 \pi^{3}} + \frac{2}{15 \pi}\right) \sin{\left (15 \pi x \right )} - \frac{1}{8 \pi} \sin{\left (16 \pi x \right )} + \left(- \frac{8}{4913 \pi^{3}} + \frac{2}{17 \pi}\right) \sin{\left (17 \pi x \right )} - \frac{1}{9 \pi} \sin{\left (18 \pi x \right )} + \left(- \frac{8}{6859 \pi^{3}} + \frac{2}{19 \pi}\right) \sin{\left (19 \pi x \right )}$$




```python
#17 is an even function
x = symbols("x")
def fourier(k,l):
    a0 = integrate(1-x,(x,0,l))/l
    expr = a0
    strb = 'b1:'+str(k)
    symb  = symbols(strb)    
    j=1
    for i in symb:
        i = 2*integrate((1-x)*cos(j*pi*x/l),(x,0,1))/l
        expr += i*cos(j*pi*x/l)
        j+=1
    
    return expr

plt.show(plot(fourier(20,1),(x,-2,2)))
```


    
![png](output_34_0.png)
    



```python
expr = fourier(20,1)
expr.evalf(subs={x:.5})
```




$$0.5$$




```python
#20
def even(k,l):
    a0 = integrate(x*x,(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i = 2*integrate(x*x*cos(j*pi*x/l), (x,0,l))/l
        expr += i*cos(j*pi*x/l)
        j+=1
    return expr
expr =even(20,2)
expr.expand()
```




$$- \frac{16}{\pi^{2}} \cos{\left (\frac{\pi x}{2} \right )} + \frac{4}{\pi^{2}} \cos{\left (\pi x \right )} - \frac{16}{9 \pi^{2}} \cos{\left (\frac{3 \pi}{2} x \right )} + \frac{1}{\pi^{2}} \cos{\left (2 \pi x \right )} - \frac{16}{25 \pi^{2}} \cos{\left (\frac{5 \pi}{2} x \right )} + \frac{4}{9 \pi^{2}} \cos{\left (3 \pi x \right )} - \frac{16}{49 \pi^{2}} \cos{\left (\frac{7 \pi}{2} x \right )} + \frac{1}{4 \pi^{2}} \cos{\left (4 \pi x \right )} - \frac{16}{81 \pi^{2}} \cos{\left (\frac{9 \pi}{2} x \right )} + \frac{4}{25 \pi^{2}} \cos{\left (5 \pi x \right )} - \frac{16}{121 \pi^{2}} \cos{\left (\frac{11 \pi}{2} x \right )} + \frac{1}{9 \pi^{2}} \cos{\left (6 \pi x \right )} - \frac{16}{169 \pi^{2}} \cos{\left (\frac{13 \pi}{2} x \right )} + \frac{4}{49 \pi^{2}} \cos{\left (7 \pi x \right )} - \frac{16}{225 \pi^{2}} \cos{\left (\frac{15 \pi}{2} x \right )} + \frac{1}{16 \pi^{2}} \cos{\left (8 \pi x \right )} - \frac{16}{289 \pi^{2}} \cos{\left (\frac{17 \pi}{2} x \right )} + \frac{4}{81 \pi^{2}} \cos{\left (9 \pi x \right )} - \frac{16}{361 \pi^{2}} \cos{\left (\frac{19 \pi}{2} x \right )} + \frac{4}{3}$$




```python
print(float(pi**2/6))
```

    1.6449340668482264



```python
#23 a
def even(k,l):
    a0 = integrate(1,(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i= 2*integrate(cos(j*pi*x/l),(x,0,l))/l
        expr+= i*cos(j*pi*x/l)
        j+=1
    return expr
even(20,4)
```




$$1$$




```python
plot(1,(x,-8,8))
```


    
![png](output_39_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30e2a3198>




```python
def odd(k,l):
    #a0 = integrate(1,(x,0,l))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i= 2*integrate(sin(j*pi*x/l),(x,0,l))/l
        expr+= i*sin(j*pi*x/l)
        j+=1
    return expr
odd(20,4)
```




$$\frac{4}{\pi} \sin{\left (\frac{\pi x}{4} \right )} + \frac{4}{3 \pi} \sin{\left (\frac{3 \pi}{4} x \right )} + \frac{4}{5 \pi} \sin{\left (\frac{5 \pi}{4} x \right )} + \frac{4}{7 \pi} \sin{\left (\frac{7 \pi}{4} x \right )} + \frac{4}{9 \pi} \sin{\left (\frac{9 \pi}{4} x \right )} + \frac{4}{11 \pi} \sin{\left (\frac{11 \pi}{4} x \right )} + \frac{4}{13 \pi} \sin{\left (\frac{13 \pi}{4} x \right )} + \frac{4}{15 \pi} \sin{\left (\frac{15 \pi}{4} x \right )} + \frac{4}{17 \pi} \sin{\left (\frac{17 \pi}{4} x \right )} + \frac{4}{19 \pi} \sin{\left (\frac{19 \pi}{4} x \right )}$$




```python
plot(odd(20,4),(x,-8,8))
```


    
![png](output_41_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30e47c2b0>




```python
#24
def even(k,l=4):
    a0 = integrate(1,(x,2,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i= 2*integrate(cos(j*pi*x/l),(x,2,l))/l
        expr+= i*cos(j*pi*x/l)
        j+=1
    return expr
even(20,4)
```




$$- \frac{2}{\pi} \cos{\left (\frac{\pi x}{4} \right )} + \frac{2}{3 \pi} \cos{\left (\frac{3 \pi}{4} x \right )} - \frac{2}{5 \pi} \cos{\left (\frac{5 \pi}{4} x \right )} + \frac{2}{7 \pi} \cos{\left (\frac{7 \pi}{4} x \right )} - \frac{2}{9 \pi} \cos{\left (\frac{9 \pi}{4} x \right )} + \frac{2}{11 \pi} \cos{\left (\frac{11 \pi}{4} x \right )} - \frac{2}{13 \pi} \cos{\left (\frac{13 \pi}{4} x \right )} + \frac{2}{15 \pi} \cos{\left (\frac{15 \pi}{4} x \right )} - \frac{2}{17 \pi} \cos{\left (\frac{17 \pi}{4} x \right )} + \frac{2}{19 \pi} \cos{\left (\frac{19 \pi}{4} x \right )} + \frac{1}{2}$$




```python
plot(even(20),(x,-8,8))
```


    
![png](output_43_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30e101208>




```python
def odd(k,l=4):
    #a0 = integrate(1,(x,0,l))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i= 2*integrate(sin(j*pi*x/l),(x,2,l))/l
        expr+= i*sin(j*pi*x/l)
        j+=1
    return expr
odd(20,4)
```




$$\frac{2}{\pi} \sin{\left (\frac{\pi x}{4} \right )} - \frac{2}{\pi} \sin{\left (\frac{\pi x}{2} \right )} + \frac{2}{3 \pi} \sin{\left (\frac{3 \pi}{4} x \right )} + \frac{2}{5 \pi} \sin{\left (\frac{5 \pi}{4} x \right )} - \frac{2}{3 \pi} \sin{\left (\frac{3 \pi}{2} x \right )} + \frac{2}{7 \pi} \sin{\left (\frac{7 \pi}{4} x \right )} + \frac{2}{9 \pi} \sin{\left (\frac{9 \pi}{4} x \right )} - \frac{2}{5 \pi} \sin{\left (\frac{5 \pi}{2} x \right )} + \frac{2}{11 \pi} \sin{\left (\frac{11 \pi}{4} x \right )} + \frac{2}{13 \pi} \sin{\left (\frac{13 \pi}{4} x \right )} - \frac{2}{7 \pi} \sin{\left (\frac{7 \pi}{2} x \right )} + \frac{2}{15 \pi} \sin{\left (\frac{15 \pi}{4} x \right )} + \frac{2}{17 \pi} \sin{\left (\frac{17 \pi}{4} x \right )} - \frac{2}{9 \pi} \sin{\left (\frac{9 \pi}{2} x \right )} + \frac{2}{19 \pi} \sin{\left (\frac{19 \pi}{4} x \right )}$$




```python
plot(odd(40,4))
```


    
![png](output_45_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30e8ffdd8>




```python
#25
def even(k,l=pi):
    a0 = integrate(pi-x,(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i= 2*integrate((pi-x)*cos(j*pi*x/l),(x,0,l))/l
        expr+= i*cos(j*pi*x/l)
        j+=1
    return expr
even(20)
```




$$\frac{4}{\pi} \cos{\left (x \right )} + \frac{4}{9 \pi} \cos{\left (3 x \right )} + \frac{4}{25 \pi} \cos{\left (5 x \right )} + \frac{4}{49 \pi} \cos{\left (7 x \right )} + \frac{4}{81 \pi} \cos{\left (9 x \right )} + \frac{4}{121 \pi} \cos{\left (11 x \right )} + \frac{4}{169 \pi} \cos{\left (13 x \right )} + \frac{4}{225 \pi} \cos{\left (15 x \right )} + \frac{4}{289 \pi} \cos{\left (17 x \right )} + \frac{4}{361 \pi} \cos{\left (19 x \right )} + \frac{\pi}{2}$$




```python
plot(even(20))
```


    
![png](output_47_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30dd7b4a8>




```python
def odd(k,l=pi):
    #a0 = integrate(pi-x,(x,0,l))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i= 2*integrate((pi-x)*sin(j*pi*x/l),(x,0,l))/l
        expr+= i*sin(j*pi*x/l)
        j+=1
    return expr
odd(20)
```




$$2 \sin{\left (x \right )} + \sin{\left (2 x \right )} + \frac{2}{3} \sin{\left (3 x \right )} + \frac{1}{2} \sin{\left (4 x \right )} + \frac{2}{5} \sin{\left (5 x \right )} + \frac{1}{3} \sin{\left (6 x \right )} + \frac{2}{7} \sin{\left (7 x \right )} + \frac{1}{4} \sin{\left (8 x \right )} + \frac{2}{9} \sin{\left (9 x \right )} + \frac{1}{5} \sin{\left (10 x \right )} + \frac{2}{11} \sin{\left (11 x \right )} + \frac{1}{6} \sin{\left (12 x \right )} + \frac{2}{13} \sin{\left (13 x \right )} + \frac{1}{7} \sin{\left (14 x \right )} + \frac{2}{15} \sin{\left (15 x \right )} + \frac{1}{8} \sin{\left (16 x \right )} + \frac{2}{17} \sin{\left (17 x \right )} + \frac{1}{9} \sin{\left (18 x \right )} + \frac{2}{19} \sin{\left (19 x \right )}$$




```python
plot(odd(20))
```


    
![png](output_49_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30d85dc88>




```python
#26
def even(k,l=pi):
    a0 = (integrate(x,(x,0,pi/2))+integrate(pi/2,(x,pi/2,pi)))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i= 2*(integrate(x*cos(j*x),(x,0,pi/2))+integrate((pi/2)*cos(j*x),(x,pi/2,pi)))/l
        expr+= i*cos(j*pi*x/l)
        j+=1
    return expr
even(20)
```




$$- \frac{2}{\pi} \cos{\left (x \right )} - \frac{1}{\pi} \cos{\left (2 x \right )} - \frac{2}{9 \pi} \cos{\left (3 x \right )} - \frac{2}{25 \pi} \cos{\left (5 x \right )} - \frac{1}{9 \pi} \cos{\left (6 x \right )} - \frac{2}{49 \pi} \cos{\left (7 x \right )} - \frac{2}{81 \pi} \cos{\left (9 x \right )} - \frac{1}{25 \pi} \cos{\left (10 x \right )} - \frac{2}{121 \pi} \cos{\left (11 x \right )} - \frac{2}{169 \pi} \cos{\left (13 x \right )} - \frac{1}{49 \pi} \cos{\left (14 x \right )} - \frac{2}{225 \pi} \cos{\left (15 x \right )} - \frac{2}{289 \pi} \cos{\left (17 x \right )} - \frac{1}{81 \pi} \cos{\left (18 x \right )} - \frac{2}{361 \pi} \cos{\left (19 x \right )} + \frac{3 \pi}{8}$$




```python
plot(even(20))
```


    
![png](output_51_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30da21d30>




```python
def odd(k,l=pi):
    #a0 = (integrate(x,(x,0,pi/2))+integrate(pi/2,(x,pi/2,pi)))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i= 2*(integrate(x*sin(j*x),(x,0,pi/2))+integrate((pi/2)*sin(j*x),(x,pi/2,pi)))/l
        expr+= i*sin(j*pi*x/l)
        j+=1
    return expr
odd(20)
```




$$\frac{1}{\pi} \left(2 + \pi\right) \sin{\left (x \right )} - \frac{1}{2} \sin{\left (2 x \right )} + \frac{1}{\pi} \left(- \frac{2}{9} + \frac{\pi}{3}\right) \sin{\left (3 x \right )} - \frac{1}{4} \sin{\left (4 x \right )} + \frac{1}{\pi} \left(\frac{2}{25} + \frac{\pi}{5}\right) \sin{\left (5 x \right )} - \frac{1}{6} \sin{\left (6 x \right )} + \frac{1}{\pi} \left(- \frac{2}{49} + \frac{\pi}{7}\right) \sin{\left (7 x \right )} - \frac{1}{8} \sin{\left (8 x \right )} + \frac{1}{\pi} \left(\frac{2}{81} + \frac{\pi}{9}\right) \sin{\left (9 x \right )} - \frac{1}{10} \sin{\left (10 x \right )} + \frac{1}{\pi} \left(- \frac{2}{121} + \frac{\pi}{11}\right) \sin{\left (11 x \right )} - \frac{1}{12} \sin{\left (12 x \right )} + \frac{1}{\pi} \left(\frac{2}{169} + \frac{\pi}{13}\right) \sin{\left (13 x \right )} - \frac{1}{14} \sin{\left (14 x \right )} + \frac{1}{\pi} \left(- \frac{2}{225} + \frac{\pi}{15}\right) \sin{\left (15 x \right )} - \frac{1}{16} \sin{\left (16 x \right )} + \frac{1}{\pi} \left(\frac{2}{289} + \frac{\pi}{17}\right) \sin{\left (17 x \right )} - \frac{1}{18} \sin{\left (18 x \right )} + \frac{1}{\pi} \left(- \frac{2}{361} + \frac{\pi}{19}\right) \sin{\left (19 x \right )}$$




```python
plot(odd(20))
```


    
![png](output_53_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30e786588>




```python
#27
def even(k,l=pi):
    a0 = (integrate(pi/2,(x,0,pi/2))+integrate(pi-x,(x,pi/2,pi)))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i= 2*(integrate((pi/2)*cos(j*x),(x,0,pi/2))+integrate((pi-x)*cos(j*x),(x,pi/2,pi)))/l
        expr+= i*cos(j*pi*x/l)
        j+=1
    return expr
even(20)
```




$$\frac{2}{\pi} \cos{\left (x \right )} - \frac{1}{\pi} \cos{\left (2 x \right )} + \frac{2}{9 \pi} \cos{\left (3 x \right )} + \frac{2}{25 \pi} \cos{\left (5 x \right )} - \frac{1}{9 \pi} \cos{\left (6 x \right )} + \frac{2}{49 \pi} \cos{\left (7 x \right )} + \frac{2}{81 \pi} \cos{\left (9 x \right )} - \frac{1}{25 \pi} \cos{\left (10 x \right )} + \frac{2}{121 \pi} \cos{\left (11 x \right )} + \frac{2}{169 \pi} \cos{\left (13 x \right )} - \frac{1}{49 \pi} \cos{\left (14 x \right )} + \frac{2}{225 \pi} \cos{\left (15 x \right )} + \frac{2}{289 \pi} \cos{\left (17 x \right )} - \frac{1}{81 \pi} \cos{\left (18 x \right )} + \frac{2}{361 \pi} \cos{\left (19 x \right )} + \frac{3 \pi}{8}$$




```python
plot(even(20))
```


    
![png](output_55_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30d8b7978>




```python
def odd(k,l=pi):
    #a0 = (integrate(x,(x,0,pi/2))+integrate(pi/2,(x,pi/2,pi)))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i= 2*(integrate(x*sin(j*x),(x,0,pi/2))+integrate((pi/2)*sin(j*x),(x,pi/2,pi)))/l
        expr+= i*sin(j*pi*x/l)
        j+=1
    return expr
odd(20)
```




$$\frac{1}{\pi} \left(2 + \pi\right) \sin{\left (x \right )} - \frac{1}{2} \sin{\left (2 x \right )} + \frac{1}{\pi} \left(- \frac{2}{9} + \frac{\pi}{3}\right) \sin{\left (3 x \right )} - \frac{1}{4} \sin{\left (4 x \right )} + \frac{1}{\pi} \left(\frac{2}{25} + \frac{\pi}{5}\right) \sin{\left (5 x \right )} - \frac{1}{6} \sin{\left (6 x \right )} + \frac{1}{\pi} \left(- \frac{2}{49} + \frac{\pi}{7}\right) \sin{\left (7 x \right )} - \frac{1}{8} \sin{\left (8 x \right )} + \frac{1}{\pi} \left(\frac{2}{81} + \frac{\pi}{9}\right) \sin{\left (9 x \right )} - \frac{1}{10} \sin{\left (10 x \right )} + \frac{1}{\pi} \left(- \frac{2}{121} + \frac{\pi}{11}\right) \sin{\left (11 x \right )} - \frac{1}{12} \sin{\left (12 x \right )} + \frac{1}{\pi} \left(\frac{2}{169} + \frac{\pi}{13}\right) \sin{\left (13 x \right )} - \frac{1}{14} \sin{\left (14 x \right )} + \frac{1}{\pi} \left(- \frac{2}{225} + \frac{\pi}{15}\right) \sin{\left (15 x \right )} - \frac{1}{16} \sin{\left (16 x \right )} + \frac{1}{\pi} \left(\frac{2}{289} + \frac{\pi}{17}\right) \sin{\left (17 x \right )} - \frac{1}{18} \sin{\left (18 x \right )} + \frac{1}{\pi} \left(- \frac{2}{361} + \frac{\pi}{19}\right) \sin{\left (19 x \right )}$$




```python
plot(odd(20))
```


    
![png](output_57_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30da21780>




```python
#28
l,a0 = symbols('l a0')

def even(k,l):
    a0 = integrate(x,(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i= 2*integrate(x*cos(j*pi*x/l),(x,0,l))/l
        expr+= i*cos(j*pi*x/l)
        j+=1
    return expr
even(20,l)
```




$$- \frac{4 l}{\pi^{2}} \cos{\left (\frac{\pi x}{l} \right )} - \frac{4 l}{9 \pi^{2}} \cos{\left (\frac{3 \pi}{l} x \right )} - \frac{4 l}{25 \pi^{2}} \cos{\left (\frac{5 \pi}{l} x \right )} - \frac{4 l}{49 \pi^{2}} \cos{\left (\frac{7 \pi}{l} x \right )} - \frac{4 l}{81 \pi^{2}} \cos{\left (\frac{9 \pi}{l} x \right )} - \frac{4 l}{121 \pi^{2}} \cos{\left (\frac{11 \pi}{l} x \right )} - \frac{4 l}{169 \pi^{2}} \cos{\left (\frac{13 \pi}{l} x \right )} - \frac{4 l}{225 \pi^{2}} \cos{\left (\frac{15 \pi}{l} x \right )} - \frac{4 l}{289 \pi^{2}} \cos{\left (\frac{17 \pi}{l} x \right )} - \frac{4 l}{361 \pi^{2}} \cos{\left (\frac{19 \pi}{l} x \right )} + \frac{l}{2}$$




```python
def odd(k,l):
    #a0 = integrate(x,(x,0,l))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i= 2*integrate(x*sin(j*pi*x/l),(x,0,l))/l
        expr+= i*sin(j*pi*x/l)
        j+=1
    return expr
odd(20,l)
```




$$\frac{2 l}{\pi} \sin{\left (\frac{\pi x}{l} \right )} - \frac{l}{\pi} \sin{\left (\frac{2 \pi}{l} x \right )} + \frac{2 l}{3 \pi} \sin{\left (\frac{3 \pi}{l} x \right )} - \frac{l}{2 \pi} \sin{\left (\frac{4 \pi}{l} x \right )} + \frac{2 l}{5 \pi} \sin{\left (\frac{5 \pi}{l} x \right )} - \frac{l}{3 \pi} \sin{\left (\frac{6 \pi}{l} x \right )} + \frac{2 l}{7 \pi} \sin{\left (\frac{7 \pi}{l} x \right )} - \frac{l}{4 \pi} \sin{\left (\frac{8 \pi}{l} x \right )} + \frac{2 l}{9 \pi} \sin{\left (\frac{9 \pi}{l} x \right )} - \frac{l}{5 \pi} \sin{\left (\frac{10 \pi}{l} x \right )} + \frac{2 l}{11 \pi} \sin{\left (\frac{11 \pi}{l} x \right )} - \frac{l}{6 \pi} \sin{\left (\frac{12 \pi}{l} x \right )} + \frac{2 l}{13 \pi} \sin{\left (\frac{13 \pi}{l} x \right )} - \frac{l}{7 \pi} \sin{\left (\frac{14 \pi}{l} x \right )} + \frac{2 l}{15 \pi} \sin{\left (\frac{15 \pi}{l} x \right )} - \frac{l}{8 \pi} \sin{\left (\frac{16 \pi}{l} x \right )} + \frac{2 l}{17 \pi} \sin{\left (\frac{17 \pi}{l} x \right )} - \frac{l}{9 \pi} \sin{\left (\frac{18 \pi}{l} x \right )} + \frac{2 l}{19 \pi} \sin{\left (\frac{19 \pi}{l} x \right )}$$




```python
#29
def even(k,l=pi):
    a0 = integrate(sin(x),(x,0,l))/l
    expr = a0
    stra = 'a1:'+str(k)
    syma = symbols(stra)
    j=1
    for i in syma:
        i= 2*integrate(sin(x)*cos(j*pi*x/l),(x,0,l))/l
        expr+= i*cos(j*pi*x/l)
        j+=1
    return expr
even(20)
```




$$- \frac{4}{3 \pi} \cos{\left (2 x \right )} - \frac{4}{15 \pi} \cos{\left (4 x \right )} - \frac{4}{35 \pi} \cos{\left (6 x \right )} - \frac{4}{63 \pi} \cos{\left (8 x \right )} - \frac{4}{99 \pi} \cos{\left (10 x \right )} - \frac{4}{143 \pi} \cos{\left (12 x \right )} - \frac{4}{195 \pi} \cos{\left (14 x \right )} - \frac{4}{255 \pi} \cos{\left (16 x \right )} - \frac{4}{323 \pi} \cos{\left (18 x \right )} + \frac{2}{\pi}$$




```python
plot(even(20),(x,-pi,pi))
```


    
![png](output_61_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd30de97c50>




```python
def odd(k,l=pi):
    #a0 = integrate(x,(x,0,l))/l
    expr = 0
    strb = 'b1:'+str(k)
    symb = symbols(strb)
    j=1
    for i in symb:
        i= 2*integrate(sin(x)*sin(j*pi*x/l),(x,0,l))/l
        expr+= i*sin(j*pi*x/l)
        j+=1
    return expr
odd(20)
```




$$\sin{\left (x \right )}$$




```python
plot(odd(20))
```


    
![png](output_63_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fd33c25bd30>




```python
print("")
```


```python

```
