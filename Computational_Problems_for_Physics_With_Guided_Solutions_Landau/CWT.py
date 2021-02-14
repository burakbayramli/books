""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# CWT.py  Continuous Wavelet TF, a la Zlatko Dimcovic 
												
import matplotlib.pylab as p;
from mpl_toolkits.mplot3d import Axes3D ;
from visual.graph import *;

originalsignal=gdisplay(x=0, y=0, width=600, height=200, \
        title='Input Signal',xmin=0,xmax=12,ymin=-20,ymax=20)
orsigraph = gcurve(color=color.yellow)
invtrgr = gdisplay(x=0, y=200, width=600, height=200,
         title='Inverted Transform',xmin=0,xmax=12,ymin=-20,ymax=20)
invtr = gcurve(x=list(range(0,240)),display=invtrgr,color=color.green)
iT =  0.0;        fT =  12.0;       W = fT - iT;
N =  240;         h =  W/N
noPtsSig =  N;    noS =  20;        noTau =  90;
iTau =  0.;       iS =  0.1;        tau =  iTau;      s =  iS                
            
# Need *very* small s steps for high frequency;
dTau =  W/noTau;    dS =  (W/iS)**(1./noS);
maxY =  0.001;      sig =  zeros((noPtsSig), float)     # Signal
      
def signal(noPtsSig, y):                       # Signal function
    t = 0.0;     hs = W/noPtsSig;     t1 = W/6.;    t2 = 4.*W/6.
    for i in range(0, noPtsSig):  
        if  t >= iT  and t <=  t1:  y[i] =  sin(2*pi*t)
        elif t >= t1 and t <=  t2: y[i] = 5.*sin(2*pi*t) + 10.*sin(4*pi*t);
        elif t >= t2 and t <=  fT: 
             y[i] = 2.5*sin(2*pi*t) + 6.*sin(4*pi*t) + 10.*sin(6*pi*t)
        else: 
            print("In signal(...) : t out of range.")
            sys.exit(1)
        yy=y[i]
        orsigraph.plot(pos=(t,yy))
        t += hs    
signal(noPtsSig, sig)                            # Form signal
Yn =  zeros( (noS+1, noTau+1), float)            # Transform

def morlet(t, s, tau):                            # Mother  
     T =  (t - tau)/s
     return sin(8*T) * exp( - T*T/2. )
	
def transform(s, tau, sig):                   # Find wavelet TF
    integral = 0.
    t = iT;                                                     
    for i in range(0, len(sig) ):
         t += h
         integral += sig[i]*morlet(t, s, tau)*h
    return integral / sqrt(s)
          
def invTransform(t, Yn):                   # Compute inverse
    s = iS                                  # Transform
    tau = iTau                             
    recSig_t = 0                 
    for i in range (0, noS):
        s *= dS                            # Scale graph
        tau = iTau                                                   
        for j in range (0, noTau):
            tau += dTau                 
            recSig_t += dTau*dS *(s**(-1.5))* Yn[i,j] * morlet(t,s,tau)
    return recSig_t

print("working, finding transform, count 20")
for i in range( 0, noS):
    s *= dS                                        # Scaling
    tau = iT
    print(i)
    for j in range(0, noTau):
         tau += dTau                              # Translate
         Yn[i, j] = transform(s, tau, sig)
print("transform found")  
for i in range( 0, noS):
    for j in range( 0, noTau):
        if Yn[i, j] > maxY or Yn[i, j] < - 1 *maxY :
            maxY = abs( Yn[i, j] )                # Find max Y       
tau =  iT
s =  iS
print("normalize")      
for i in range( 0, noS):
     s *= dS                             
     for j in range( 0, noTau):
         tau +=   dTau                             # Transform
         Yn[i, j] = Yn[i, j]/maxY
     tau = iT
print("finding inverse transform")                 # Inverse TF
recSigData =  "recSig.dat"                   
recSig =  zeros(len(sig) )                                   
t =  0.0;
print("count to 10")
kco = 0;            j = 0;              Yinv =  Yn             
for rs in range(0, len(recSig) ):                     
    recSig[rs] = invTransform(t, Yinv)       # Find input signal
    xx=rs/20
    yy=4.6*recSig[rs]
    invtr.plot(pos=(xx,yy))
    t += h 
    if kco %24 == 0:
        j += 1
        print(j)                            
    kco += 1    
x = list(range(1, noS + 1))                             
y = list(range(1, noTau + 1))                                         
X,Y = p.meshgrid(x, y)                                    

def functz(Yn):                          # Transform function
    z = Yn[X, Y]    
    return z
                
Z = functz(Yn)                                               
fig = p.figure()                                              
ax = Axes3D(fig)                                        
ax.plot_wireframe(X, Y, Z, color = 'r')          
ax.set_xlabel('s: scale')                                         
ax.set_ylabel('Tau')
ax.set_zlabel('Transform')
p.show()

print("Done")                                   
