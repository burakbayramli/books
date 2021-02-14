""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

''' Uses global variables nedeed to perform wavelet or inverse 
 Wavelet Discrete Transform, Daubechies type'''
 
from visual import *
from visual.graph import *
N=1024;             # number of data points must be, 4,8,16,32,...,1024..
'''  
Function tht returns  the chirp signal sin(60 t*t)
generally for 0<=t <=0 (selected in main program)
xi is the variable time
'''
sq3 = math.sqrt(3)    
fsq2 = 4.0*math.sqrt(2)
c0 = (1.0+sq3)/fsq2                            # Daubechies 4 coefficents
c1 = (3.0+sq3)/fsq2
c2 = (3.0-sq3)/fsq2
c3 = (1.0-sq3)/fsq2
def chirp( xi):
    y = math.sin(60.0*xi**2);
    return y;
'''            
Continues the Discret Wavelet Transform, Daubechies 4
or the inverse Daubechies 4 transform
sign:>=0 for DWT,
sign:<0 for inverse transform
'''
def daube4(f,n, sign):
    tr = zeros((n+1),float)                          # temporary variable
    #print "n",n
    if n < 4 :
        return
    mp = n/2                                          # midpoint of array
    mp1 = mp+1                                        # midpoint plus one
    if sign >= 0:                                                   # DWT
        j = 1
        i = 1
        maxx = n/2
        if n >  128:                                  # appropiate scales
            maxy = 3.0
            miny = -3.0
            Maxy = 0.2
            Miny = -0.2
            speed = 50                                        # fast rate
        else:
            maxy=10.0
            miny=-5.0
            Maxy=7.5
            Miny=-7.5
            speed=8               #for lower rate
        transfgr1 = gdisplay(x=0,y=0,width=600,height=400, title='Wavelet Transform, down sampling, Low pass filters',
                          xmax=maxx,xmin=0,ymax=maxy,ymin=miny)
        #notice that the width of the bars (delta)  changes with scale
        transf = gvbars(delta=2.0*n/N,color=color.cyan,display=transfgr1)     
        transfgr2 = gdisplay(x=0,y=400,width=600,height=400, title='Wavelet Transform, down sampling, High pass filters',
                          xmax=2*maxx,xmin=0,ymax=Maxy,ymin=Miny)
        transf2 = gvbars(delta=2.*n/N,color=color.cyan,display=transfgr2)
        
        while j<=n-3:
            rate(speed)
            
            tr[i] = c0*f[j]+c1*f[j+1]+c2*f[j+2]+c3*f[j+3]      # low-pass
            transf.plot(pos=(i,tr[i]) )#c coefficients
            tr[i+mp] = c3*f[j]-c2*f[j+1]+c1*f[j+2]-c0*f[j+3]  # high-pass
            transf2.plot(pos=(i+mp,tr[i+mp]))
            i += 1                                         #d coefficents
            j += 2                                    # downsampling here
        #l ast data
        tr[i] = c0*f[n-1]+c1*f[n]+c2*f[1]+c3*f[2]       # low-pass filter
        transf.plot(pos=(i,tr[i]) )#c coefficients
        tr[i+mp] = c3*f[n-1]-c2*f[n]+c1*f[1]-c0*f[2]   # high-pass filter
        transf2.plot(pos=(i+mp,tr[i+mp]))
    else:                              #inverse DWT 
        tr[1] = c2*f[mp]+c1*f[n]+c0*f[1]+c3*f[mp1]   # low-pass first one
        tr[2] = c3*f[mp]-c0*f[n]+c1*f[1]-c2*f[mp1]    # high-pass 2nd one
        
        for i in range (1,mp):
             if i == 1:
               j = 3
             tr[j] = c2*f[i]+c1*f[i+mp]+c0*f[i+1]+c3*f[i+mp1]  # low-pass
             j += 1           #upsamplig  c coefficients
             tr[j] = c3*f[i]-c0*f[i+mp]+c1*f[i+1]-c2*f[i+mp1] # high-pass
             j += 1;          #upsampling  d coefficients
    for i in range(1,n+1):
        f[i] = tr[i]      #copy transform in data array
       
'''  
Calls wavelet transform routine daube4  (Daubechies 4 wavelet D4)
with sign=1 to perform the D4 discrete Wavelet transform
or with sign=-1 the inverse D4 wavelet trnsform
f the input data, it hs to have 2**n data (4,8,16,32,...)
'''
def pyram(f, n, sign):
    #Discrete wavelet transform. Replaces f by its wavelet transform 
    if (n < 4):
        return                                             # too few data
    nend=4
    #this variable indicates when to stop the loop
    #or can be selected as 512,256,128,64,..,4 t
    if sign >= 0 :                                    # Wavelet transform
        nd = n
        while nd >= nend:    # Performs filtering operations downsampling
            daube4(f,nd,sign)
            nd /= 2  
    else:                                     # Inverse wavelet transform
        nd = 4
        while nd <= n+1: # perform upsampling
            daube4(f,nd,sign)
            nd *= 2   
f = zeros((N+1),float)                                     # data vector
inxi = 1.0/N                  # for the chirp signal interval 0=<t <=1.0
#selects initial data 
xi = 0.0
for i in range(1,N+1):
    f[i] = chirp(xi)                 # change if you use another function
    xi += inxi;          # different of chirp or if need for next 2 lines
    #f[i] = 0.0;         if instead of chirp to find formation of Wavelet
    #f[5] = 1.0;         used with this line too
    #w.println(" "+xi+" "+f[i]+" ")           # copy in file "indata.dat"
n=N       # total number of datapoints it must be 4,8,16,32,...,1024,...
pyram(f,n,1)                                  # Discret Wavelet Transform
#pyram(f,n,-1)                                             # i nverse DWT
