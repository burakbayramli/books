""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# CWT_zd.java  Continuous Wavelet Transform. Written by Zlatko Dimcovic

'''Different wavelets (middle) used during transform, and transform (top) 
for each wavelet. Then wavelet translated and scaled.'''
 
from visual import *
from visual.graph import *
N = 240
transfgr = display(x=0,y=0,width=600,height=200, title='Transform, not normalized')
transf   = curve(x=list(range(0,90)),display=transfgr,color=color.cyan)
wavlgr   = display(x=0,y=200,width=600,height=200,
                title='Morlet Wavelet at different scales, up to s=12.0')
wavelet  = curve(x=list(range(0,N)),display=wavlgr,color=color.yellow)
invtrgr  = display(x=0,y=400,width=600,height=200,
                title='Inverse TF, not normalized')
invtr    = curve(x=list(range(0,N)),display=invtrgr,color=color.green)
wvlabel  = label(pos=(0, -50), text='s=', box=0,display=wavlgr)

iT       = 0.0
fT       = 12.0
W        = fT - iT                           # i,f times
h        = W/N;                              # Steps
noPtsSig = N
noS      = 30    
noTau    = 90                           # of pts
iTau     = 0.
iS       = 0.1
tau      = iTau
s        = iS

''' Need *very* small s steps for high-frequency, but only if s is small
Thus increment s by multiplying by number close enough to 1 '''

dTau = W/noTau
dS   = (W/iS)**(1./noS)
sig  = zeros((noPtsSig),float)                                   # Signal
Y    = zeros((noS,noTau),float)                               # Transform
maxY = 0.001;

def signal(noPtsSig, y):                                     # The signal
    t = 0.0
    hs = W / noPtsSig
    t1 = W/6.
    t2 = 4.*W/6.
    for i in range(0,noPtsSig):  
        if   t >= iT and t <= t1: y[i] = sin(2*pi*t)
        elif t >= t1 and t <= t2:	y[i] = 5.*sin(2*pi*t) +10.*sin(4*pi*t);
        elif t >= t2 and t <= fT: y[i] = 2.5*sin(2*pi*t)+6.*sin(4*pi*t)+10.*sin(6*pi*t)
        else:
            print("In signal(...) : t out of range.")
            sys.exit(1)
        t += hs    
signal(noPtsSig, sig)

def morlet(t, s, tau):                                           # Mother
     T = (t-tau)/s
     return sin(8*T) * exp(-T*T/2.)

def transform(s, tau, sig, j):            
    integral = 0.
    t = 0.0                             # "initial time" = class variable
    if j%2==0:   wvlabel.text = 's=%5.3f' %s
    for i in range(0,len(sig)):
         t +=h
         yy=morlet(t,s,tau)
         if j%2==0:
           wavelet.x[i]=110.0/3*t-240           # the transform are drawn
           wavelet.y[i]=40.0*yy
         integral += sig[i]*yy*h
         output=integral/sqrt(s)        
    return output

def invTransform(t,Y):        # given the transform (from previous steps)
    s = iS                                     # computes original signal
    tau = iTau
    recSig_t = 0                 
    for i in range (0,noS):
        s  *= dS                 
        tau = iTau
        for j in range (0,noTau):
            tau      += dTau
            recSig_t += dTau*dS *(s**(-1.5))* Y[i,j] * morlet(t,s,tau)
    return recSig_t
		
print("working, finding transform")
for i in range( 0, noS):
    rate(150)
    s  *= dS                                                  # Scaling s
    tau = 0.0
    for j in range(0,noTau):
         tau        += dTau                                 # Translation
         Y[i,j]      = transform(s, tau, sig,i)
         transf.x[j] = 40./3.0*tau-80
         transf.y[j] = 4.0*Y[i,j]      
print("transform found")
print("finding inverse transform")		                       # Inverse TF
recSigData = "recSig.dat"                   
recSig     = zeros(len(sig))                            # Same resolution
t   = 0.0;
kco = 0
j   = 0
for rs in range(0, len(recSig)):   			         # with inverse transform
    recSig[rs] = invTransform(t, Y) 			     # find the original signal
    t += h                          		                 # not normalized
    invtr.x[rs] = rs*2.0-220
    invtr.y[rs] = 1.5*recSig[rs]
    
print("nDone")                                   
