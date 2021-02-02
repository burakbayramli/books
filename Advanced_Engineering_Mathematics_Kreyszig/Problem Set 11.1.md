```python
%matplotlib inline
```

    Matplotlib is building the font cache; this may take a moment.



```python
#6
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- np.pi,np.pi,400)
y = []
for i in x:
    if i<0:
        y.append(-i)
    else:
        y.append(i)
y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fcc810217c0>]




    
![png](output_1_1.png)
    



```python
#7.a
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(- np.pi,np.pi,400)
y = []
for i in x:
    if i<0:
        y.append(-np.sin(i))
    else:
        y.append(np.sin(i))
y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fcc80f06b80>]




    
![png](output_2_1.png)
    



```python
#7.b
x = np.linspace(- np.pi,np.pi,400)
y = []
for i in x:
    if i<0:
        y.append(np.sin(-i))
    else:
        y.append(np.sin(i))
y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fcc80ee4dc0>]




    
![png](output_3_1.png)
    



```python
#8.a
x = np.linspace(- np.pi,np.pi,400)
y = []
for i in x:
    if i<0:
        y.append(np.exp(i))
    else:
        y.append(np.exp(-i))
y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fcc80ec3be0>]




    
![png](output_4_1.png)
    



```python
#8.b
x = np.linspace(- np.pi,np.pi,400)
y = []
for i in x:
    y.append(np.exp(-i))
y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fcc80e1c850>]




    
![png](output_5_1.png)
    



```python
#9
x = np.linspace(- np.pi,np.pi,400)
y = []
for i in x:
    if i<0:
        y.append(i)
    else:
        y.append(np.pi-i)
y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fcc80defeb0>]




    
![png](output_6_1.png)
    



```python
#10
x = np.linspace(- np.pi,np.pi,400)
y = []
for i in x:
    if i<0:
        y.append(-(np.cos(i))**2)
    else:
        y.append((np.cos(i))**2)
y = np.array(y)
plt.plot(x,y,'-')
```




    [<matplotlib.lines.Line2D at 0x7fcc80d55130>]




    
![png](output_7_1.png)
    



```python
# 12
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = (integrate(-x,(x,-pi,0))+integrate(x, (x,0,pi)))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = (integrate(-x*cos(j*x),(x,-pi,0))+
            integrate(x*cos(j*x),(x,0,pi)))/pi
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i =  (integrate(-x*sin(j*x),(x,-pi,0))+
            integrate(x*sin(j*x),(x,0,pi)))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
#print(expand(expr))
plot(expr1(20),(x,-2*pi,2*pi))
```


    
![png](output_8_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc80e6bd90>




```python
expr1(20)
```




$\displaystyle - \frac{4 \cos{\left(x \right)}}{\pi} - \frac{4 \cos{\left(3 x \right)}}{9 \pi} - \frac{4 \cos{\left(5 x \right)}}{25 \pi} - \frac{4 \cos{\left(7 x \right)}}{49 \pi} - \frac{4 \cos{\left(9 x \right)}}{81 \pi} - \frac{4 \cos{\left(11 x \right)}}{121 \pi} - \frac{4 \cos{\left(13 x \right)}}{169 \pi} - \frac{4 \cos{\left(15 x \right)}}{225 \pi} - \frac{4 \cos{\left(17 x \right)}}{289 \pi} - \frac{4 \cos{\left(19 x \right)}}{361 \pi} + \frac{\pi}{2}$




```python
# 13
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = (integrate(x,(x,-pi,0))+integrate(pi-x, (x,0,pi)))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = (integrate(x*cos(j*x),(x,-pi,0))+
            integrate((pi-x)*cos(j*x),(x,0,pi)))/pi
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i =  (integrate(x*sin(j*x),(x,-pi,0))+
            integrate((pi-x)*sin(j*x),(x,0,pi)))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
plot(expr1(50),(x,-2*pi,2*pi))
```


    
![png](output_10_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc78d8c2e0>




```python
expr1(50)
```




$\displaystyle 2 \sin{\left(x \right)} + \frac{2 \sin{\left(3 x \right)}}{3} + \frac{2 \sin{\left(5 x \right)}}{5} + \frac{2 \sin{\left(7 x \right)}}{7} + \frac{2 \sin{\left(9 x \right)}}{9} + \frac{2 \sin{\left(11 x \right)}}{11} + \frac{2 \sin{\left(13 x \right)}}{13} + \frac{2 \sin{\left(15 x \right)}}{15} + \frac{2 \sin{\left(17 x \right)}}{17} + \frac{2 \sin{\left(19 x \right)}}{19} + \frac{2 \sin{\left(21 x \right)}}{21} + \frac{2 \sin{\left(23 x \right)}}{23} + \frac{2 \sin{\left(25 x \right)}}{25} + \frac{2 \sin{\left(27 x \right)}}{27} + \frac{2 \sin{\left(29 x \right)}}{29} + \frac{2 \sin{\left(31 x \right)}}{31} + \frac{2 \sin{\left(33 x \right)}}{33} + \frac{2 \sin{\left(35 x \right)}}{35} + \frac{2 \sin{\left(37 x \right)}}{37} + \frac{2 \sin{\left(39 x \right)}}{39} + \frac{2 \sin{\left(41 x \right)}}{41} + \frac{2 \sin{\left(43 x \right)}}{43} + \frac{2 \sin{\left(45 x \right)}}{45} + \frac{2 \sin{\left(47 x \right)}}{47} + \frac{2 \sin{\left(49 x \right)}}{49} + \frac{4 \cos{\left(x \right)}}{\pi} + \frac{4 \cos{\left(3 x \right)}}{9 \pi} + \frac{4 \cos{\left(5 x \right)}}{25 \pi} + \frac{4 \cos{\left(7 x \right)}}{49 \pi} + \frac{4 \cos{\left(9 x \right)}}{81 \pi} + \frac{4 \cos{\left(11 x \right)}}{121 \pi} + \frac{4 \cos{\left(13 x \right)}}{169 \pi} + \frac{4 \cos{\left(15 x \right)}}{225 \pi} + \frac{4 \cos{\left(17 x \right)}}{289 \pi} + \frac{4 \cos{\left(19 x \right)}}{361 \pi} + \frac{4 \cos{\left(21 x \right)}}{441 \pi} + \frac{4 \cos{\left(23 x \right)}}{529 \pi} + \frac{4 \cos{\left(25 x \right)}}{625 \pi} + \frac{4 \cos{\left(27 x \right)}}{729 \pi} + \frac{4 \cos{\left(29 x \right)}}{841 \pi} + \frac{4 \cos{\left(31 x \right)}}{961 \pi} + \frac{4 \cos{\left(33 x \right)}}{1089 \pi} + \frac{4 \cos{\left(35 x \right)}}{1225 \pi} + \frac{4 \cos{\left(37 x \right)}}{1369 \pi} + \frac{4 \cos{\left(39 x \right)}}{1521 \pi} + \frac{4 \cos{\left(41 x \right)}}{1681 \pi} + \frac{4 \cos{\left(43 x \right)}}{1849 \pi} + \frac{4 \cos{\left(45 x \right)}}{2025 \pi} + \frac{4 \cos{\left(47 x \right)}}{2209 \pi} + \frac{4 \cos{\left(49 x \right)}}{2401 \pi}$




```python
# 14
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = integrate(x**2, (x,-pi,pi))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = integrate(x**2*cos(j*x),(x,-pi,pi))/pi
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i =  integrate(x**2*sin(j*x),(x,-pi,pi))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
plot(expr1(20),(x,-2*pi,2*pi))
```


    
![png](output_12_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc78afe040>




```python
expr1(50)
```




$\displaystyle - 4 \cos{\left(x \right)} + \cos{\left(2 x \right)} - \frac{4 \cos{\left(3 x \right)}}{9} + \frac{\cos{\left(4 x \right)}}{4} - \frac{4 \cos{\left(5 x \right)}}{25} + \frac{\cos{\left(6 x \right)}}{9} - \frac{4 \cos{\left(7 x \right)}}{49} + \frac{\cos{\left(8 x \right)}}{16} - \frac{4 \cos{\left(9 x \right)}}{81} + \frac{\cos{\left(10 x \right)}}{25} - \frac{4 \cos{\left(11 x \right)}}{121} + \frac{\cos{\left(12 x \right)}}{36} - \frac{4 \cos{\left(13 x \right)}}{169} + \frac{\cos{\left(14 x \right)}}{49} - \frac{4 \cos{\left(15 x \right)}}{225} + \frac{\cos{\left(16 x \right)}}{64} - \frac{4 \cos{\left(17 x \right)}}{289} + \frac{\cos{\left(18 x \right)}}{81} - \frac{4 \cos{\left(19 x \right)}}{361} + \frac{\cos{\left(20 x \right)}}{100} - \frac{4 \cos{\left(21 x \right)}}{441} + \frac{\cos{\left(22 x \right)}}{121} - \frac{4 \cos{\left(23 x \right)}}{529} + \frac{\cos{\left(24 x \right)}}{144} - \frac{4 \cos{\left(25 x \right)}}{625} + \frac{\cos{\left(26 x \right)}}{169} - \frac{4 \cos{\left(27 x \right)}}{729} + \frac{\cos{\left(28 x \right)}}{196} - \frac{4 \cos{\left(29 x \right)}}{841} + \frac{\cos{\left(30 x \right)}}{225} - \frac{4 \cos{\left(31 x \right)}}{961} + \frac{\cos{\left(32 x \right)}}{256} - \frac{4 \cos{\left(33 x \right)}}{1089} + \frac{\cos{\left(34 x \right)}}{289} - \frac{4 \cos{\left(35 x \right)}}{1225} + \frac{\cos{\left(36 x \right)}}{324} - \frac{4 \cos{\left(37 x \right)}}{1369} + \frac{\cos{\left(38 x \right)}}{361} - \frac{4 \cos{\left(39 x \right)}}{1521} + \frac{\cos{\left(40 x \right)}}{400} - \frac{4 \cos{\left(41 x \right)}}{1681} + \frac{\cos{\left(42 x \right)}}{441} - \frac{4 \cos{\left(43 x \right)}}{1849} + \frac{\cos{\left(44 x \right)}}{484} - \frac{4 \cos{\left(45 x \right)}}{2025} + \frac{\cos{\left(46 x \right)}}{529} - \frac{4 \cos{\left(47 x \right)}}{2209} + \frac{\cos{\left(48 x \right)}}{576} - \frac{4 \cos{\left(49 x \right)}}{2401} + \frac{\pi^{2}}{3}$




```python
# 15
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = (integrate((2*pi+x)**2,(x,-pi,0))
          +integrate(x**2, (x,0,pi)))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = (integrate((2*pi+x)**2*cos(j*x),(x,-pi,0))+
            integrate((x**2)*cos(j*x),(x,0,pi)))/pi
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i =  (integrate((2*pi+x)**2*sin(j*x),(x,-pi,0))+
            integrate((x**2)*sin(j*x),(x,0,pi)))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
plot(expr1(20),(x,-2*pi,2*pi))
```


    
![png](output_14_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc785d70d0>




```python
expr1(20)
```




$\displaystyle - 4 \pi \sin{\left(x \right)} - 2 \pi \sin{\left(2 x \right)} - \frac{4 \pi \sin{\left(3 x \right)}}{3} - \pi \sin{\left(4 x \right)} - \frac{4 \pi \sin{\left(5 x \right)}}{5} - \frac{2 \pi \sin{\left(6 x \right)}}{3} - \frac{4 \pi \sin{\left(7 x \right)}}{7} - \frac{\pi \sin{\left(8 x \right)}}{2} - \frac{4 \pi \sin{\left(9 x \right)}}{9} - \frac{2 \pi \sin{\left(10 x \right)}}{5} - \frac{4 \pi \sin{\left(11 x \right)}}{11} - \frac{\pi \sin{\left(12 x \right)}}{3} - \frac{4 \pi \sin{\left(13 x \right)}}{13} - \frac{2 \pi \sin{\left(14 x \right)}}{7} - \frac{4 \pi \sin{\left(15 x \right)}}{15} - \frac{\pi \sin{\left(16 x \right)}}{4} - \frac{4 \pi \sin{\left(17 x \right)}}{17} - \frac{2 \pi \sin{\left(18 x \right)}}{9} - \frac{4 \pi \sin{\left(19 x \right)}}{19} + 4 \cos{\left(x \right)} + \cos{\left(2 x \right)} + \frac{4 \cos{\left(3 x \right)}}{9} + \frac{\cos{\left(4 x \right)}}{4} + \frac{4 \cos{\left(5 x \right)}}{25} + \frac{\cos{\left(6 x \right)}}{9} + \frac{4 \cos{\left(7 x \right)}}{49} + \frac{\cos{\left(8 x \right)}}{16} + \frac{4 \cos{\left(9 x \right)}}{81} + \frac{\cos{\left(10 x \right)}}{25} + \frac{4 \cos{\left(11 x \right)}}{121} + \frac{\cos{\left(12 x \right)}}{36} + \frac{4 \cos{\left(13 x \right)}}{169} + \frac{\cos{\left(14 x \right)}}{49} + \frac{4 \cos{\left(15 x \right)}}{225} + \frac{\cos{\left(16 x \right)}}{64} + \frac{4 \cos{\left(17 x \right)}}{289} + \frac{\cos{\left(18 x \right)}}{81} + \frac{4 \cos{\left(19 x \right)}}{361} + \frac{4 \pi^{2}}{3}$




```python
#16
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = integrate(x,(x,-pi/2,pi/2))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = integrate(x*cos(j*x),(x,-pi/2,pi/2))/pi
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i = integrate(x*sin(j*x),(x,-pi/2,pi/2))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
plot(expr1(40),(x,-2*pi,2*pi))
```


    
![png](output_16_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc783e0a30>




```python
expr1(40)
```




$\displaystyle \frac{2 \sin{\left(x \right)}}{\pi} + \frac{\sin{\left(2 x \right)}}{2} - \frac{2 \sin{\left(3 x \right)}}{9 \pi} - \frac{\sin{\left(4 x \right)}}{4} + \frac{2 \sin{\left(5 x \right)}}{25 \pi} + \frac{\sin{\left(6 x \right)}}{6} - \frac{2 \sin{\left(7 x \right)}}{49 \pi} - \frac{\sin{\left(8 x \right)}}{8} + \frac{2 \sin{\left(9 x \right)}}{81 \pi} + \frac{\sin{\left(10 x \right)}}{10} - \frac{2 \sin{\left(11 x \right)}}{121 \pi} - \frac{\sin{\left(12 x \right)}}{12} + \frac{2 \sin{\left(13 x \right)}}{169 \pi} + \frac{\sin{\left(14 x \right)}}{14} - \frac{2 \sin{\left(15 x \right)}}{225 \pi} - \frac{\sin{\left(16 x \right)}}{16} + \frac{2 \sin{\left(17 x \right)}}{289 \pi} + \frac{\sin{\left(18 x \right)}}{18} - \frac{2 \sin{\left(19 x \right)}}{361 \pi} - \frac{\sin{\left(20 x \right)}}{20} + \frac{2 \sin{\left(21 x \right)}}{441 \pi} + \frac{\sin{\left(22 x \right)}}{22} - \frac{2 \sin{\left(23 x \right)}}{529 \pi} - \frac{\sin{\left(24 x \right)}}{24} + \frac{2 \sin{\left(25 x \right)}}{625 \pi} + \frac{\sin{\left(26 x \right)}}{26} - \frac{2 \sin{\left(27 x \right)}}{729 \pi} - \frac{\sin{\left(28 x \right)}}{28} + \frac{2 \sin{\left(29 x \right)}}{841 \pi} + \frac{\sin{\left(30 x \right)}}{30} - \frac{2 \sin{\left(31 x \right)}}{961 \pi} - \frac{\sin{\left(32 x \right)}}{32} + \frac{2 \sin{\left(33 x \right)}}{1089 \pi} + \frac{\sin{\left(34 x \right)}}{34} - \frac{2 \sin{\left(35 x \right)}}{1225 \pi} - \frac{\sin{\left(36 x \right)}}{36} + \frac{2 \sin{\left(37 x \right)}}{1369 \pi} + \frac{\sin{\left(38 x \right)}}{38} - \frac{2 \sin{\left(39 x \right)}}{1521 \pi}$




```python
#17
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = (integrate(pi+x,(x,-pi,0))
          +integrate(pi-x, (x,0,pi)))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = (integrate((pi+x)*cos(j*x),(x,-pi,0))+
            integrate((pi-x)*cos(j*x),(x,0,pi)))/pi
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i =  (integrate((pi+x)*sin(j*x),(x,-pi,0))+
            integrate((pi-x)*sin(j*x),(x,0,pi)))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
plot(expr1(20),(x,-2*pi,2*pi))
```


    
![png](output_18_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc78a139a0>




```python
expr1(20)
```




$\displaystyle \frac{4 \cos{\left(x \right)}}{\pi} + \frac{4 \cos{\left(3 x \right)}}{9 \pi} + \frac{4 \cos{\left(5 x \right)}}{25 \pi} + \frac{4 \cos{\left(7 x \right)}}{49 \pi} + \frac{4 \cos{\left(9 x \right)}}{81 \pi} + \frac{4 \cos{\left(11 x \right)}}{121 \pi} + \frac{4 \cos{\left(13 x \right)}}{169 \pi} + \frac{4 \cos{\left(15 x \right)}}{225 \pi} + \frac{4 \cos{\left(17 x \right)}}{289 \pi} + \frac{4 \cos{\left(19 x \right)}}{361 \pi} + \frac{\pi}{2}$




```python
# 18
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = integrate(1,(x,0,pi))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = integrate(1*cos(j*x),(x,0,pi))/pi
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i = integrate(1*sin(j*x),(x,0,pi))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
plot(expr1(20),(x,-2*pi,2*pi))
```


    
![png](output_20_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc78299ca0>




```python
expr1(20)
```




$\displaystyle \frac{2 \sin{\left(x \right)}}{\pi} + \frac{2 \sin{\left(3 x \right)}}{3 \pi} + \frac{2 \sin{\left(5 x \right)}}{5 \pi} + \frac{2 \sin{\left(7 x \right)}}{7 \pi} + \frac{2 \sin{\left(9 x \right)}}{9 \pi} + \frac{2 \sin{\left(11 x \right)}}{11 \pi} + \frac{2 \sin{\left(13 x \right)}}{13 \pi} + \frac{2 \sin{\left(15 x \right)}}{15 \pi} + \frac{2 \sin{\left(17 x \right)}}{17 \pi} + \frac{2 \sin{\left(19 x \right)}}{19 \pi} + \frac{1}{2}$




```python
# 19
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = integrate(x,(x,0,pi))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = integrate(x*cos(j*x),(x,0,pi))/pi
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i = integrate(x*sin(j*x),(x,0,pi))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
plot(expr1(20),(x,-2*pi,2*pi))
```


    
![png](output_22_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc7b00e3a0>




```python
expr1(20)
```




$\displaystyle \sin{\left(x \right)} - \frac{\sin{\left(2 x \right)}}{2} + \frac{\sin{\left(3 x \right)}}{3} - \frac{\sin{\left(4 x \right)}}{4} + \frac{\sin{\left(5 x \right)}}{5} - \frac{\sin{\left(6 x \right)}}{6} + \frac{\sin{\left(7 x \right)}}{7} - \frac{\sin{\left(8 x \right)}}{8} + \frac{\sin{\left(9 x \right)}}{9} - \frac{\sin{\left(10 x \right)}}{10} + \frac{\sin{\left(11 x \right)}}{11} - \frac{\sin{\left(12 x \right)}}{12} + \frac{\sin{\left(13 x \right)}}{13} - \frac{\sin{\left(14 x \right)}}{14} + \frac{\sin{\left(15 x \right)}}{15} - \frac{\sin{\left(16 x \right)}}{16} + \frac{\sin{\left(17 x \right)}}{17} - \frac{\sin{\left(18 x \right)}}{18} + \frac{\sin{\left(19 x \right)}}{19} - \frac{2 \cos{\left(x \right)}}{\pi} - \frac{2 \cos{\left(3 x \right)}}{9 \pi} - \frac{2 \cos{\left(5 x \right)}}{25 \pi} - \frac{2 \cos{\left(7 x \right)}}{49 \pi} - \frac{2 \cos{\left(9 x \right)}}{81 \pi} - \frac{2 \cos{\left(11 x \right)}}{121 \pi} - \frac{2 \cos{\left(13 x \right)}}{169 \pi} - \frac{2 \cos{\left(15 x \right)}}{225 \pi} - \frac{2 \cos{\left(17 x \right)}}{289 \pi} - \frac{2 \cos{\left(19 x \right)}}{361 \pi} + \frac{\pi}{4}$




```python
#20
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = (integrate(-pi/2,(x,-pi,-pi/2))
          +integrate(x,(x,-pi/2,pi/2))
         + integrate(pi/2,(x,pi/2,pi)))/(2*pi) 
    expr = a0
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = (integrate(-pi*cos(j*x)/2,(x,-pi,-pi/2))
          +integrate(x*cos(j*x),(x,-pi/2,pi/2))
         + integrate(pi*cos(j*x)/2,(x,pi/2,pi)))/pi 
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i = (integrate(-pi*sin(j*x)/2,(x,-pi,-pi/2))
          +integrate(x*sin(j*x),(x,-pi/2,pi/2))
         + integrate(pi*sin(j*x)/2,(x,pi/2,pi)))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
plot(expr1(40),(x,-2*pi,2*pi))
```


    
![png](output_24_0.png)
    





    <sympy.plotting.plot.Plot at 0x7fcc7b072d90>




```python
#21
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)

x = symbols('x')

def expr1(k):
    x = symbols('x')
    a0 = (integrate(-pi-x,(x,-pi,0))
          + integrate(pi-x,(x,0,pi)))/(2*pi) 
    expr = a0
    #print(float(a0))
    stra = 'a1:'+ str(k)
    strb = 'b1:'+ str(k)
    syma = symbols(stra)
    symb = symbols(strb)
    j = 1
    for i in syma:
        i = (integrate((-pi-x)*cos(j*x),(x,-pi,0))
          + integrate((pi-x)*cos(j*x),(x,0,pi)))/pi 
        expr += i*cos(j*x)
        j+=1
    j = 1
    for i in symb:
        i = (integrate((-pi-x)*sin(j*x),(x,-pi,0))
          + integrate((pi-x)*sin(j*x),(x,0,pi)))/pi
        expr += i*sin(j*x)
        j+=1
    return expr
#print(expr)
plt.show(plot(expr1(40),(x,-2*pi,2*pi)))
expr1(40)
```


    
![png](output_25_0.png)
    





$\displaystyle 2 \sin{\left(x \right)} + \sin{\left(2 x \right)} + \frac{2 \sin{\left(3 x \right)}}{3} + \frac{\sin{\left(4 x \right)}}{2} + \frac{2 \sin{\left(5 x \right)}}{5} + \frac{\sin{\left(6 x \right)}}{3} + \frac{2 \sin{\left(7 x \right)}}{7} + \frac{\sin{\left(8 x \right)}}{4} + \frac{2 \sin{\left(9 x \right)}}{9} + \frac{\sin{\left(10 x \right)}}{5} + \frac{2 \sin{\left(11 x \right)}}{11} + \frac{\sin{\left(12 x \right)}}{6} + \frac{2 \sin{\left(13 x \right)}}{13} + \frac{\sin{\left(14 x \right)}}{7} + \frac{2 \sin{\left(15 x \right)}}{15} + \frac{\sin{\left(16 x \right)}}{8} + \frac{2 \sin{\left(17 x \right)}}{17} + \frac{\sin{\left(18 x \right)}}{9} + \frac{2 \sin{\left(19 x \right)}}{19} + \frac{\sin{\left(20 x \right)}}{10} + \frac{2 \sin{\left(21 x \right)}}{21} + \frac{\sin{\left(22 x \right)}}{11} + \frac{2 \sin{\left(23 x \right)}}{23} + \frac{\sin{\left(24 x \right)}}{12} + \frac{2 \sin{\left(25 x \right)}}{25} + \frac{\sin{\left(26 x \right)}}{13} + \frac{2 \sin{\left(27 x \right)}}{27} + \frac{\sin{\left(28 x \right)}}{14} + \frac{2 \sin{\left(29 x \right)}}{29} + \frac{\sin{\left(30 x \right)}}{15} + \frac{2 \sin{\left(31 x \right)}}{31} + \frac{\sin{\left(32 x \right)}}{16} + \frac{2 \sin{\left(33 x \right)}}{33} + \frac{\sin{\left(34 x \right)}}{17} + \frac{2 \sin{\left(35 x \right)}}{35} + \frac{\sin{\left(36 x \right)}}{18} + \frac{2 \sin{\left(37 x \right)}}{37} + \frac{\sin{\left(38 x \right)}}{19} + \frac{2 \sin{\left(39 x \right)}}{39}$




```python
#22.a
x = symbols('x')
expr = 0
for i in range(1,20):
    if i%2 == 0:
        expr += -sin(i*x)
    else:
        expr += sin(i*x)
expr *= 2
plt.show(plot(expr,(x,-4*pi,4*pi)))
```


    
![png](output_26_0.png)
    



```python
#22.b
x = symbols('x')
expr = 0
for i in range(1,20,2):
    expr += cos(i*x)/i**2
expr *= 4/pi**2
expr += 0.5
plt.show(plot(expr,(x,-2*pi,2*pi)))
```


    
![png](output_27_0.png)
    



```python
#22.c
x = symbols('x')
expr = 0
for i in range(1,20):
    expr += (-1**(i+1))*cos(i*x)/i**2
expr *= 4
expr += 2*(pi**2)/3
plt.show(plot(expr,(x,-2*pi,2*pi)))
```


    
![png](output_28_0.png)
    



```python

```


```python

```
