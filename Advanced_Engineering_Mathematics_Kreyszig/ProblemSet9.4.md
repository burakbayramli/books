```python
from sympy import *
from sympy.plotting import plot,plot3d,plot_parametric
import matplotlib.pyplot as plt
init_printing(use_unicode= True)
```


```python
x,y,z = symbols('x y z')
p1 = plot_implicit(Eq(x**2 -y**2,5))
p2 = plot_implicit(Eq(x*y,5))
p3 = plot_implicit(Eq(3*x-4*y,4))
p4 = plot_implicit(Eq(tanh(y/x),1))
p5 = plot_implicit(Eq(y/(x**2+y**2),.5))
p6 = plot_implicit(Eq(x/(x**2+y**2),.5))
p7 = plot_implicit(Eq(9*x**2 + 4*y**2,25))
```


    
![png](output_1_0.png)
    



    
![png](output_1_1.png)
    



    
![png](output_1_2.png)
    



    
![png](output_1_3.png)
    



    
![png](output_1_4.png)
    



    
![png](output_1_5.png)
    



    
![png](output_1_6.png)
    



```python
p8a = plot_implicit(Eq(x**2 -4*x-y**2,1))
p8b = plot_implicit(Eq(y*x**2 - y**3/3,5))
p8c = plot_implicit(Eq(cos(x)*sinh(y),.5))
p8d = plot_implicit(Eq(sin(x)*sinh(y),.5))
p8e = plot_implicit(Eq(exp(x)*sin(y),5))
p8f = plot_implicit(Eq(exp(2*x)*cos(2*y),5))
p8g = plot_implicit(Eq(x**4 - 6*(y**2)*x**2 + y**4,5))
p8h = plot_implicit(Eq(x**2 - 2*x - y**2,5))
```


    
![png](output_2_0.png)
    



    
![png](output_2_1.png)
    



    
![png](output_2_2.png)
    



    
![png](output_2_3.png)
    



    
![png](output_2_4.png)
    



    
![png](output_2_5.png)
    



    
![png](output_2_6.png)
    



    
![png](output_2_7.png)
    



```python
plot3d(-2*x + 1.5*y)
plot3d((9 - 9*(x**2+y**2))**.5)
plot3d(5*x**2 +2*y**2)
plot3d(sqrt(x**2+y**2))
plot3d(x**2 + y**2)
plot3d(x- y**2)
```


    
![png](output_3_0.png)
    



    
![png](output_3_1.png)
    



    
![png](output_3_2.png)
    



    
![png](output_3_3.png)
    



    
![png](output_3_4.png)
    



    
![png](output_3_5.png)
    





    <sympy.plotting.plot.Plot at 0x7fa9be7331d0>




```python
from sympy.vector import CoordSys3D
N = CoordSys3D('N')
```


```python

```


```python

```


```python

```


```python

```
