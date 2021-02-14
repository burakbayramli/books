""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ScattSqWell.py: Quantum scattering from square well
import scipy.special
import matplotlib.pyplot as plt
import numpy as np
from math import * 

a = 1;  V = 15;  E = 10; nLs = 10                                  # Constants
ninpts = 100;  Npsi = 100                                        # Pts for psi  
alpha  = np.sqrt(V+E);   beta = np.sqrt(E) 
delta  = np.zeros((nLs),float)                                  # Phase shifts
SigL = np.zeros((nLs,200),float)                       # Partial cross section
     
def Gam(n,xx):
    gamma = np.zeros((n),float)
    for nn in range(0,n):
       jn,jpr = scipy.special.sph_jn(nn,xx)  # Spherical Bessel Functions
    gamma = alpha*jpr/jn   # gamma match owavefunctions outside-inside
    return gamma 
    
def phaseshifts(n,alpha,beta):
    gamm = Gam(n,alpha)
    num = np.zeros((n),float); den = np.zeros((n),float)
    jnb,jnpr =   scipy.special.sph_jn(n,beta)
    ynb,yprb =  scipy.special.sph_yn(n,beta)
    for i in range(0,n):
        num1=gamm[i]*jnb[i]
        den1=gamm[i]*ynb[i]
        num[i]=beta*jnpr[i]-num1
        den[i]=beta*yprb[i]-den1  
        td=atan2(num[i],den[i])      
        delta[i]=td
    return delta
def totalcrossect(n,alpha,beta):
    delta=phaseshifts(n,alpha,beta)
    suma=0
    for i in range (0,n):
        suma=suma+(2*i+1)*(sin(delta[i]))**2
        
    return 4*np.pi*suma/beta**2
    
def plotcross(alpha,beta): 
    e=0.
    cross=np.zeros((200),float) # for total crossection
    delta=phaseshifts(n,alpha,beta)
    en=np.zeros((200),float)# energies
    for i in range (1,200):
       e=e+100/300.
       en[i]=e
       alpha=np.sqrt(V+e)
       beta=np.sqrt(e)   
       cross[i]=totalcrossect(n,alpha,beta)
       for m in range(0,n):
           partot[m,i]=4*pi*(2*m+1)*(sin(delta[m]))**2/beta**2
    f2=plt.figure()
    ax2=f2.add_subplot(111)       
    plt.plot(en,cross,label="total")
    plt.plot(en,partot[0,:],label="S ")
    plt.plot(en,partot[1,:], label ="P ")
    plt.plot(en,partot[2,:],label="D")
    plt.plot(en,partot[3,:],label="E")
    plt.title("Total cross section and contribution of each $\delta$= S,P, D, E")
    plt.legend()
    plt.xlabel("Energy")
def diffcrossection():
        zz2=np.zeros((n),complex)
        dcr=np.zeros((180),float)
        delta=phaseshifts(n,alpha,beta)  # phaseshifts
        for i in range(0,n):     # n partial waves
            cosd=cos(delta[i])
            sind=sin(delta[i])
            zz=complex(cosd,sind)
            zz2[i]=zz*sind
        for ang in range(0,180):
            summ=0.
            radi=cos(ang*pi/180.)  # convert degrees to radians
            for i in range(0,n):# for each partial wave
                poL=scipy.special.eval_legendre(i,radi)# call legendre pols
                summ+= (2*i+1)*zz2[i]*poL
            dcr[ang]=(summ.real**2 +summ.imag**2)/beta**2#diffcrossection
        angu=np.arange(0,180)  # angle in degrees  
        f1=plt.figure() # to plot a separate figure
        ax1=f1.add_subplot(111)
        plt.semilogy(angu,dcr)   #plot semilog diffcrossection
        plt.xlabel("scattering angle")
        plt.title ("Differential cross section")
        plt.grid()

def wavefunction():  # computes internal r<1 and externalr>1 wavef.
     delta=phaseshifts(n,alpha,beta)
     BL=np.zeros((n),complex)
     Rin=np.zeros((n,ninpts),float) #internal wavefunction r<1 for n partwaves
     Rex=np.zeros((n,nexpts),float)
     for i in range (0,10):     # to find BL for matching
          jnb,jnpr=scipy.special.sph_jn(n,alpha)# SphBessel functions
          jnf,jnfr=scipy.special.sph_jn(n,beta)
          ynb,yprb=r=scipy.special.sph_yn(n,beta)
          cosd=cos(delta[i])
          sind=sin(delta[i])
          zz=complex(cosd,-sind)
          num=jnb[i]*zz
          den=cosd*jnf[i]-sind*ynb[i]
          BL[i]=num/den  # to match internal and external wavefunctions
     intr=1.0/ninpts # 50points inside, increment          
     for i in range(0,n):   # internal wavefunc 
          rin=intr
          for ri in range(0,ninpts): #plot internal func
              alpr=alpha*rin
              jnint,jnintpr=scipy.special.sph_jn(n,alpr)
              Rin[i,ri]=rin*jnint[i]
              rin=rin+intr
     extr=2./nexpts       # from r= 1 to 3   
     for i in range(0,n):   
          rex=1.0
          for rx in range(0,nexpts): #plot internal func
              argu=beta*rex
              jnxt,jnintpr=scipy.special.sph_jn(n,argu)
              nxt,jnintpr=scipy.special.sph_yn(n,argu)
              factr=jnxt[i]*cos(delta[i])-nxt[i]*sin(delta[i])
              fsin=sin(delta[i])*factr
              fcos=cos(delta[i])*factr
              Rex[i,rx]=rex*(fcos*BL.real[i]-fsin*BL.imag[i] )
              
              rex=rex+extr       
     ai=np.arange(0,1,intr)       
     nwaf=0  #partial wavef to plotCHANGE FOR OTHER WAVES 1 2 3..
     f3=plt.figure()
     ax3=f3.add_subplot(111)
     plt.plot(ai,Rin[nwaf, :])  # only plot s wavefunction
     ae=np.arange(1,3,extr)   
     plt.title("  r * partial wavefunction S ")
     plt.xlabel ("r")
     plt.plot(ae,Rex[nwaf, :])  
diffcrossection()        # on graph with diffcrossection 
plotcross(alpha,beta)   # other graph total crossection +partialconttrib           
wavefunction()  # s wave function internal and external 
plt.show()    

