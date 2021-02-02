```python
from sympy import *
from sympy.plotting import plot
init_printing(use_unicode= True)
```


```python
x = symbols('x')
def error(k):
    a0 = integrate(x+pi, (x,-pi,pi))/(2*pi)
    subractor = 2*a0**2
    stra = 'a1:'+str(k+1)
    strb = 'b1:'+str(k+1)
    syma = symbols(stra)
    symb = symbols(strb)
    j =1
    for i in syma:
        i = integrate((x+pi)*cos(j*x),(x,-pi,pi))/pi
        j+=1
        subractor += i**2
    j=1
    for i in symb:
        i = integrate((x+pi)*sin(j*x),(x,-pi,pi))/pi
        j+=1
        subractor += i**2
    E = integrate((x+pi)**2,(x,-pi,pi))
    subractor *= -pi
    E += subractor
    return float(E)

for i in range(1,11):
    print(i," ", error(i))
for i in range(20,71,10):
    print(i," ", error(i))
```

    1   8.104480505840707
    2   4.962887852250914
    3   3.5666244506554503
    4   2.781226287258002
    5   2.278571462683635
    6   1.9295056122847691
    7   1.6730490691345818
    8   1.4766995282852198
    9   1.3215591503301682
    10   1.1958954441865766
    20   0.6128722361710509
    30   0.4119752564354791
    40   0.3102649953766761
    50   0.24883089198510067
    60   0.2077038767223724
    70   0.17824340334835775



```python
#2
x = symbols('x')
def error1(k= 100):
    a0 = integrate(x, (x,-pi,pi))/(2*pi)
    stra = 'a1:'+str(k+1)
    strb = 'b1:'+str(k+1)
    syma = symbols(stra)
    symb = symbols(strb)
    A =[]
    B=[]
    j = 1
    for i in syma:
        i = integrate(x*cos(j*x),(x,-pi,pi))/pi
        #print(i)
        A.append(i)
        j+=1
    j=1
    for i in symb:
        i = integrate(x*sin(j*x),(x,-pi,pi))/pi
        #print(i)
        B.append(i)
        j+=1
    return a0,A,B

a0, syma, symb = error1()

E = integrate(x**2,(x,-pi,pi))
for i in range(1,11):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 +symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

for i in range(20,101,10):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 +symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

expr = a0
for i in range(1,10):
    expr += syma[i-1]*cos(i*x)
    expr += symb[i-1]*sin(i*x)

expr
```

    1   8.104480505840707
    2   4.962887852250914
    3   3.5666244506554503
    4   2.781226287258002
    5   2.278571462683635
    6   1.9295056122847691
    7   1.6730490691345818
    8   1.4766995282852198
    9   1.3215591503301682
    10   1.1958954441865766
    20   0.6128722361710509
    30   0.4119752564354791
    40   0.3102649953766761
    50   0.24883089198510067
    60   0.2077038767223724
    70   0.17824340334835775
    80   0.15610197546285975
    90   0.13885351116880196
    100   0.12503748196609127





$$2 \sin{\left (x \right )} - \sin{\left (2 x \right )} + \frac{2}{3} \sin{\left (3 x \right )} - \frac{1}{2} \sin{\left (4 x \right )} + \frac{2}{5} \sin{\left (5 x \right )} - \frac{1}{3} \sin{\left (6 x \right )} + \frac{2}{7} \sin{\left (7 x \right )} - \frac{1}{4} \sin{\left (8 x \right )} + \frac{2}{9} \sin{\left (9 x \right )}$$




```python
#3
x = symbols('x')
def error1(k= 100):
    a0 = (integrate(-x, (x,-pi,0))+integrate(x, (x,0,pi)))/(2*pi)
    stra = 'a1:'+str(k+1)
    strb = 'b1:'+str(k+1)
    syma = symbols(stra)
    symb = symbols(strb)
    A =[]
    B=[]
    j = 1
    for i in syma:
        i = (integrate(-x*cos(j*x), (x,-pi,0))+integrate(x*cos(j*x), (x,0,pi)))/pi
        #print(i)
        A.append(i)
        j+=1
    j=1
    for i in symb:
        i = (integrate(-x*sin(j*x), (x,-pi,0))+integrate(x*sin(j*x), (x,0,pi)))/pi
        #print(i)
        B.append(i)
        j+=1
    return a0,A,B

a0, syma, symb = error1()

E = integrate(x**2,(x,-pi,pi))
for i in range(1,11):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 +symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

for i in range(20,101,10):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 +symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

expr = a0
for i in range(1,10):
    expr += syma[i-1]*cos(i*x)
    expr += symb[i-1]*sin(i*x)

expr
```

    1   0.07475460110931928
    2   0.07475460110931928
    3   0.011878574208817423
    4   0.011878574208817423
    5   0.0037298411225123824
    6   0.0037298411225123824
    7   0.0016086590404879547
    8   0.0016086590404879547
    9   0.0008324117948027466
    10   0.0008324117948027466
    20   0.0001055773539402246
    30   3.136842112017113e-05
    40   1.3246369417120683e-05
    50   6.785186004116662e-06
    60   3.927570604981388e-06
    70   2.4737030629454734e-06
    80   1.6573461911088714e-06
    90   1.164083493685426e-06
    100   8.486566572669366e-07





$$- \frac{4}{\pi} \cos{\left (x \right )} - \frac{4}{9 \pi} \cos{\left (3 x \right )} - \frac{4}{25 \pi} \cos{\left (5 x \right )} - \frac{4}{49 \pi} \cos{\left (7 x \right )} - \frac{4}{81 \pi} \cos{\left (9 x \right )} + \frac{\pi}{2}$$




```python
#4
x = symbols('x')
def error1(k= 100):
    a0 = integrate(x**2, (x,-pi,pi))/(2*pi)
    stra = 'a1:'+str(k+1)
    strb = 'b1:'+str(k+1)
    syma = symbols(stra)
    symb = symbols(strb)
    A =[]
    B=[]
    j = 1
    for i in syma:
        i = integrate((x**2)*cos(j*x),(x,-pi,pi))/pi
        #print(i)
        A.append(i)
        j+=1
    j=1
    for i in symb:
        i = integrate((x**2)*sin(j*x),(x,-pi,pi))/pi
        #print(i)
        B.append(i)
        j+=1
    return a0,A,B

a0, syma, symb = error1()

E = integrate(x**4,(x,-pi,pi))
for i in range(1,11):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 + symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

for i in range(20,101,10):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 + symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

expr = a0
for i in range(1,10):
    expr += syma[i-1]*cos(i*x)
    expr += symb[i-1]*sin(i*x)

expr
```

    1   4.1380170599466775
    2   0.9964244063568845
    3   0.3758628945366783
    4   0.17951335368731627
    5   0.09908858175541756
    6   0.06030348726665468
    7   0.0393682592543945
    8   0.02709641295130937
    9   0.019435159718961148
    10   0.014408611473217477
    20   0.001942544934179902
    30   0.0005902225665235322
    40   0.00025214548428391365
    50   0.00013007365375360187
    60   7.565247853623425e-05
    70   4.7812072100539564e-05
    80   3.2116444029641406e-05
    90   2.2603534361498853e-05
    100   1.6505508839175658e-05





$$- 4 \cos{\left (x \right )} + \cos{\left (2 x \right )} - \frac{4}{9} \cos{\left (3 x \right )} + \frac{1}{4} \cos{\left (4 x \right )} - \frac{4}{25} \cos{\left (5 x \right )} + \frac{1}{9} \cos{\left (6 x \right )} - \frac{4}{49} \cos{\left (7 x \right )} + \frac{1}{16} \cos{\left (8 x \right )} - \frac{4}{81} \cos{\left (9 x \right )} + \frac{\pi^{2}}{3}$$




```python
#5
x = symbols('x')
def error1(k= 100):
    a0 = (integrate(-1, (x,-pi,0))+integrate(1, (x,0,pi)))/(2*pi)
    stra = 'a1:'+str(k+1)
    strb = 'b1:'+str(k+1)
    syma = symbols(stra)
    symb = symbols(strb)
    A =[]
    B=[]
    j = 1
    for i in syma:
        i = (integrate(-1*cos(j*x), (x,-pi,0))+integrate(1*cos(j*x), (x,0,pi)))/pi
        #print(i)
        A.append(i)
        j+=1
    j=1
    for i in symb:
        i = (integrate(-1*sin(j*x), (x,-pi,0))+integrate(1*sin(j*x), (x,0,pi)))/pi
        #print(i)
        B.append(i)
        j+=1
    return a0,A,B

a0, syma, symb = error1()

E = integrate(1,(x,-pi,pi))
for i in range(1,11):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 +symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

for i in range(20,101,10):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 +symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

expr = a0
for i in range(1,10):
    expr += syma[i-1]*cos(i*x)
    expr += symb[i-1]*sin(i*x)

expr
```

    1   1.1902271282389356
    2   1.1902271282389356
    3   0.624342886134419
    4   0.624342886134419
    5   0.42062455897679296
    6   0.42062455897679296
    7   0.316686636957596
    8   0.316686636957596
    9   0.25381061005709415
    10   0.25381061005709415
    20   0.12721821964404578
    30   0.08485124703500482
    40   0.06364872590701987
    50   0.050922794976442726
    60   0.042437389933053536
    70   0.036375798700894676
    80   0.031829331116867185
    90   0.02829304793552806
    100   0.025463942187138218





$$\frac{4}{\pi} \sin{\left (x \right )} + \frac{4}{3 \pi} \sin{\left (3 x \right )} + \frac{4}{5 \pi} \sin{\left (5 x \right )} + \frac{4}{7 \pi} \sin{\left (7 x \right )} + \frac{4}{9 \pi} \sin{\left (9 x \right )}$$




```python
#7
x = symbols('x')
def error1(k= 100):
    a0 = integrate(x**3, (x,-pi,pi))/(2*pi)
    stra = 'a1:'+str(k+1)
    strb = 'b1:'+str(k+1)
    syma = symbols(stra)
    symb = symbols(strb)
    A =[]
    B=[]
    j = 1
    for i in syma:
        i = integrate((x**3)*cos(j*x),(x,-pi,pi))/pi
        #print(i)
        A.append(i)
        j+=1
    j=1
    for i in symb:
        i = integrate((x**3)*sin(j*x),(x,-pi,pi))/pi
        #print(i)
        B.append(i)
        j+=1
    return a0,A,B

a0, syma, symb = error1()

E = integrate(x**6,(x,-pi,pi))
for i in range(1,11):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 + symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

for i in range(20,101,10):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 + symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

expr = a0
for i in range(1,10):
    expr += syma[i-1]*cos(i*x)
    expr += symb[i-1]*sin(i*x)

expr
```

    1   674.7741216182759
    2   454.7046834033169
    3   336.4494629826604
    4   265.6477720471685
    5   219.03695161267498
    6   186.17344875847706
    7   161.8082720101163
    8   143.0436707874971
    9   128.15757695372432
    10   116.0651673010364
    20   59.64183591165004
    30   40.112662891579845
    40   30.215166280496184
    50   24.234539958274183
    60   20.230005967247376
    70   17.36111229619719
    80   15.204800635416866
    90   13.524925060907671
    100   12.17929876379323





$$\frac{1}{\pi} \left(- 12 \pi + 2 \pi^{3}\right) \sin{\left (x \right )} + \frac{1}{\pi} \left(- \pi^{3} + \frac{3 \pi}{2}\right) \sin{\left (2 x \right )} + \frac{1}{\pi} \left(- \frac{4 \pi}{9} + \frac{2 \pi^{3}}{3}\right) \sin{\left (3 x \right )} + \frac{1}{\pi} \left(- \frac{\pi^{3}}{2} + \frac{3 \pi}{16}\right) \sin{\left (4 x \right )} + \frac{1}{\pi} \left(- \frac{12 \pi}{125} + \frac{2 \pi^{3}}{5}\right) \sin{\left (5 x \right )} + \frac{1}{\pi} \left(- \frac{\pi^{3}}{3} + \frac{\pi}{18}\right) \sin{\left (6 x \right )} + \frac{1}{\pi} \left(- \frac{12 \pi}{343} + \frac{2 \pi^{3}}{7}\right) \sin{\left (7 x \right )} + \frac{1}{\pi} \left(- \frac{\pi^{3}}{4} + \frac{3 \pi}{128}\right) \sin{\left (8 x \right )} + \frac{1}{\pi} \left(- \frac{4 \pi}{243} + \frac{2 \pi^{3}}{9}\right) \sin{\left (9 x \right )}$$




```python
x = symbols('x')
def error1(k= 100):
    a0 = (integrate(-sin(x), (x,-pi,0))+integrate(sin(x), (x,0,pi)))/(2*pi)
    stra = 'a1:'+str(k+1)
    strb = 'b1:'+str(k+1)
    syma = symbols(stra)
    symb = symbols(strb)
    A =[]
    B=[]
    j = 1
    for i in syma:
        i = (integrate(-sin(x)*cos(j*x), (x,-pi,0))+integrate(sin(x)*cos(j*x), (x,0,pi)))/pi
        #print(i)
        A.append(i)
        j+=1
    j=1
    for i in symb:
        i = (integrate(-sin(x)*sin(j*x), (x,-pi,0))+integrate(sin(x)*sin(j*x), (x,0,pi)))/pi
        #print(i)
        B.append(i)
        j+=1
    return a0,A,B

a0, syma, symb = error1()

E = integrate((sin(x))**2,(x,-pi,pi))
for i in range(1,11):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 +symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

for i in range(20,101,10):
    Subractor = 2*a0**2
    for j in range(1,i+1):
        Subractor += syma[j-1]**2 +symb[j-1]**2
    Subractor *= -pi
    print(i ," ",float(E + Subractor))

expr = a0
for i in range(1,10):
    expr += syma[i-1]*cos(i*x)
    expr += symb[i-1]*sin(i*x)

expr
```

    1   0.5951135641194678
    2   0.029229322014951115
    3   0.029229322014951115
    4   0.006593952330770447
    5   0.006593952330770447
    6   0.0024364354500025684
    7   0.0024364354500025684
    8   0.001153251227543347
    9   0.001153251227543347
    10   0.0006336146415887861
    20   9.149081029141645e-05
    30   2.8469062221240643e-05
    40   1.2310080888380406e-05
    50   6.396978329606092e-06
    60   3.7388328046267003e-06
    70   2.371236792962964e-06
    80   1.5970218868265099e-06
    90   1.1262965123725265e-06
    100   8.237979131349093e-07





$$- \frac{4}{3 \pi} \cos{\left (2 x \right )} - \frac{4}{15 \pi} \cos{\left (4 x \right )} - \frac{4}{35 \pi} \cos{\left (6 x \right )} - \frac{4}{63 \pi} \cos{\left (8 x \right )} + \frac{2}{\pi}$$




```python

```
