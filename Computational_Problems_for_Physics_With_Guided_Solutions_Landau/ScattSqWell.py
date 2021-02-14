""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ScattSqWell.py: Quantum scattering from square well

import scipy.special, matplotlib.pyplot as plt,  numpy as np
from math import * 
a = 1;  V = 15;  E = 10; nLs = 10;  Nin = 100;  Nout = 100                             
alpha = np.sqrt(V+E);  beta = np.sqrt(E) 
delta  = np.zeros((nLs),float);  SigL = np.zeros((nLs,200),float)   
     
def Gam(n,xx): # Spherical Bessel ratio
    gamma = np.zeros((n),float)
    for nn in range(0,n):  jn,jpr = scipy.special.sph_jn(nn,xx)  
    gamma = alpha*jpr/jn   # gamma match psi outside-inside
    return gamma 
    
def phaseshifts(n,alpha,beta):
    gamm = Gam(n,alpha)
    num = np.zeros((n),float); den = np.zeros((n),float)
    jnb,jnpr = scipy.special.sph_jn(n,beta)
    ynb,yprb = scipy.special.sph_yn(n,beta)
    for i in range(0,n):
        num1 = gamm[i]*jnb[i]
        den1 = gamm[i]*ynb[i]
        num[i] = beta*jnpr[i]-num1
        den[i] = beta*yprb[i]-den1  
        td = atan2(num[i],den[i])      
        delta[i] = td
    return delta
    
def totalcrossect(n,alpha,beta):
    delta = phaseshifts(n,alpha,beta)
    suma  =  0
    for i in range (0,n): suma = suma+(2*i+1)*(sin(delta[i]))**2
    return 4*np.pi*suma/beta**2
    
def plotcross(alpha,beta): 
    e = 0.
    cross = np.zeros((200),float)         # total crossection
    delta = phaseshifts(n,alpha,beta)
    en = np.zeros((200),float)                    # energies
    for i in range (1,200):
       e = e + 100/300.
       en[i] = e
       alpha = np.sqrt(V+e)
       beta = np.sqrt(e)   
       cross[i] = totalcrossect(n,alpha,beta)
       for m in range(0,n):
           partot[m,i] = 4*pi*(2*m+1)*(sin(delta[m]))**2/beta**2
    f2 = plt.figure()
    ax2 = f2.add_subplot(111)       
    plt.plot(en,cross,label = "Total")
    plt.plot(en,partot[0,:],label = "S ")
    plt.plot(en,partot[1,:], label ="P ")
    plt.plot(en,partot[2,:],label = "D")
    plt.plot(en,partot[3,:],label = "E")
    plt.title("Total & Partial Cross Sections")
    plt.legend()
    plt.xlabel("Energy")
    
def diffcrossection():
        zz2 = np.zeros((n),complex)
        dcr = np.zeros((180),float)
        delta = phaseshifts(n,alpha,beta)     # phaseshifts
        for i in range(0,n):               # n partial waves
            cosd = cos(delta[i])
            sind = sin(delta[i])
            zz = complex(cosd,sind)
            zz2[i] = zz*sind
        for ang in range(0,180):
            summ = 0.
            radi = cos(ang*pi/180.)   
            for i in range(0,n):       #  partial wave loop
                poL = scipy.special.eval_legendre(i,radi) 
                summ+= (2*i+1)*zz2[i]*poL
            dcr[ang] = (summ.real**2 +summ.imag**2)/beta**2
        angu = np.arange(0,180)    
        f1 = plt.figure()          # plot separate figure
        ax1 = f1.add_subplot(111)
        plt.semilogy(angu,dcr)       # Semilog dsig/dw plot
        plt.xlabel("Scattering Angle")
        plt.title ("Differential Cross Section")
        plt.grid()

def wavefunction():             # Compute Psi(<1) & Psi(>1)
     delta = phaseshifts(n,alpha,beta)
     BL = np.zeros((n),complex)
     Rin = np.zeros((n,Nin),float)       # Psi(r<1), nLs
     Rex = np.zeros((n,nexpts),float)
     for i in range (0,10):            # BL for matching
          jnb,jnpr = scipy.special.sph_jn(n,alpha) # SphBes
          jnf,jnfr = scipy.special.sph_jn(n,beta)
          ynb,yprb = r = scipy.special.sph_yn(n,beta)
          cosd = cos(delta[i])
          sind = sin(delta[i])
          zz = complex(cosd,-sind)
          num = jnb[i]*zz
          den = cosd*jnf[i]-sind*ynb[i]
          BL[i] = num/den      # For wavefunction match
     intr = 1.0/Nin                  # Points increment          
     for i in range(0,n):                # Internal Psi 
          rin = intr
          for ri in range(0,Nin):          # PsiIn plot
              alpr = alpha*rin
              jnint,jnintpr = scipy.special.sph_jn(n,alpr)
              Rin[i,ri] = rin*jnint[i]
              rin = rin+intr
     extr = 2./nexpts             
     for i in range(0,n):   
          rex = 1.0
          for rx in range(0,nexpts):     # PsiIn plot
              argu = beta*rex
              jnxt,jnintpr = scipy.special.sph_jn(n,argu)
              nxt,jnintpr = scipy.special.sph_yn(n,argu)
              factr = jnxt[i]*cos(delta[i])-nxt[i]*sin(delta[i])
              fsin = sin(delta[i])*factr
              fcos = cos(delta[i])*factr
              Rex[i,rx] = rex*(fcos*BL.real[i]-fsin*BL.imag[i] )
              rex = rex+extr       
     ai = np.arange(0,1,intr)       
     nwaf = 0     # PsiL to plot, CHANGE FOR OTHER WAVES
     f3 = plt.figure()
     ax3 = f3.add_subplot(111)
     plt.plot(ai,Rin[nwaf, :])   
     ae = np.arange(1,3,extr)   
     plt.title("$\Psi(r<1), \ \ \Psi(r>1), \ \ \ell = 0$")
     plt.xlabel ("$r$")
     plt.plot(ae,Rex[nwaf, :])  

diffcrossection()               # Diff crossection 
plotcross(alpha,beta)          # Total crossections    
wavefunction()                 # Psi