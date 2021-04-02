"""
    file: STRUCTURED MESH
    A simple toolbox for structured meshes
    Reference: Klaus A. Hoffmann and Steve T. Chiang, Computational Fluid
        Dynamics, volume I, 4th edition, published by Engineering Education
        System, 2000
"""
import numpy as np
from numpy import pi as pi

# ---------------------------------------------------------------
#                   STRUCTUREDMESH - BASIC SHAPES
# ---------------------------------------------------------------

class UNIFORM:
    """
    Reference: Hoffmann Eq (9.24)
    
    p_bottom: array of nodes attached to the body
    p_top: corresponding array of nodes of the (usually) unstructured mesh surface
    p_whatever[:,0] = x-coordinate of the nodes
    p_whatever[:,1] = y-coordinate of the nodes
    """
    def __init__(self,p_top,p_bottom):
        self.p_top, self.p_bottom = p_top, p_bottom

    def __call__(self,M):
        p_top, p_bottom =  self.p_top, self.p_bottom
        len_top = len(p_top)
        len_bottom = len(p_bottom)
        if len_top != len_bottom:
            print ''
            print 'ERROR: top and bottom arrays have different length'
            print ''
        N = len_top
        
        eta = np.linspace(0,M-1,M)
        x = np.zeros((N,M))
        y = np.zeros((N,M))
        r = np.zeros((M,2))
        
        for i in range(N):
            r[:,0] = p_bottom[i,0] + ((p_top[i,0] - p_bottom[i,0])/float(M-1))*eta
            r[:,1] = p_bottom[i,1] + ((p_top[i,1] - p_bottom[i,1])/float(M-1))*eta
            x[i,:] = r[:,0]
            y[i,:] = r[:,1]            
        return x,y

class ONE_SIDED:
    """
    Reference: Hoffmann Eq (9.34), with the following replacements:
    a) replace y/H by (y - H1)/(H2 - H1)
    b) replace eta by by eta/(M-1)

    beta: a parameter that controls the clustering. Range of beta: (1,infinity)
          As beta approaches -> 1 the grid gets clustered more and more to the
          body surface. Hoffmann uses beta = 1.05 and 1.2 in his examples
    """
    def __init__(self,p_top,p_bottom):
        self.p_top, self.p_bottom = p_top, p_bottom

    def __call__(self,M,beta=1.05):
        p_top, p_bottom =  self.p_top, self.p_bottom
        len_top = len(p_top)
        len_bottom = len(p_bottom)
        if len_top != len_bottom:
            print ''
            print 'ERROR: top and bottom arrays have different length'
            print ''
        N = len_top
        
        eta = np.linspace(0,M-1,M)
        x = np.zeros((N,M))
        y = np.zeros((N,M))
        r = np.zeros((M,2))

        expo = 1.0 - eta/(M-1)
        num = (beta+1.0) - (beta-1.0)*((beta+1.0)/(beta-1.0))**expo
        denom = ((beta+1.0)/(beta-1.0))**expo + 1.0
        for i in range(N):
            H1 = p_bottom[i]
            H2 = p_top[i]
            r[:,0] = H1[0] + (H2[0] - H1[0])*(num/denom)
            r[:,1] = H1[1] + (H2[1] - H1[1])*(num/denom)
            x[i,:] = r[:,0]
            y[i,:] = r[:,1]      
        return x,y

class TWO_SIDED:
    """
    Reference: Hoffmann Eq (9.42)
    
    alpha: a parameter that controls where the clusterinr takes place. When
           alpha = 0 clustering is at the top. When alpha = 0.5 clustering is
           at the top and at the bottom. Range of alpha: (1,infinity)
           
    beta: a parameter that controls the clustering. Range of beta: (1,infinity)
          As beta approaches -> 1 the grid gets clustered more and more.
          Hoffmann uses beta = 1.05 and 1.2 in his examples
    """
    def __init__(self,p_top,p_bottom):
        self.p_top, self.p_bottom = p_top, p_bottom

    def __call__(self,M,alpha=0.5,beta=1.05):
        p_top, p_bottom =  self.p_top, self.p_bottom
        len_top = len(p_top)
        len_bottom = len(p_bottom)
        if len_top != len_bottom:
            print ''
            print 'ERROR: top and bottom arrays have different length'
            print ''
        N = len_top
        
        eta = np.linspace(0,M-1,M)
        x = np.zeros((N,M))
        y = np.zeros((N,M))
        r = np.zeros((M,2))
        
        expo = (eta/(M-1) - alpha)/(1.0 - alpha)
        num = (2.0*alpha + beta)*((beta + 1.0)/(beta - 1.0))**expo + 2.0*alpha - beta
        denom = (2.0*alpha + 1.0)*(1.0 + ((beta + 1.0)/(beta - 1.0))**expo)

        for i in range(N):
            H1 = p_bottom[i]
            H2 = p_top[i]
            r[:,0] = H1[0] + (H2[0] - H1[0])*(num/denom)
            r[:,1] = H1[1] + (H2[1] - H1[1])*(num/denom)
            x[i,:] = r[:,0]
            y[i,:] = r[:,1]

        return x,y

class INTERIOR:
    """
    Reference: Hoffmann Eq (9.50)

    Clustering in the interior of the grid

    D: coordinate where clustering is desired. Range of D: (0,1). When D = 0.5
       clustering is in the middle of the domain. When D < 0.5 clustering is
       closer to the body surface (bottom).
           
    beta: a parameter that controls the clustering. Range of beta: (0,infinity)
          As beta approaches -> 1 the grid gets clustered more and more.
          
    Hoffmann uses D = 0.5 and beta = 5 and 10 in his examples
    """
    def __init__(self,p_top,p_bottom):
        self.p_top, self.p_bottom = p_top, p_bottom

    def __call__(self,M,D=0.5,beta=5):
        p_top, p_bottom =  self.p_top, self.p_bottom
        len_top = len(p_top)
        len_bottom = len(p_bottom)
        if len_top != len_bottom:
            print ''
            print 'ERROR: top and bottom arrays have different length'
            print ''
        N = len_top
        
        eta = np.linspace(0,M-1,M)
        x = np.zeros((N,M))
        y = np.zeros((N,M))
        r = np.zeros((M,2))
        
        for i in range(N):
            H1 = p_bottom[i]
            H2 = p_top[i]
            Anum = 1.0 + (np.exp(beta) - 1.0)*D
            Adenom = 1.0 + (np.exp(-beta) - 1.0)*D
            A = (0.5/beta)*np.log(Anum/Adenom)
            num = np.sinh(beta*(eta/(M-1) - A))
            denom = np.sinh(beta*A)
            r[:,0] = H1[0] + (H2[0] - H1[0])*D*(1.0 + num/denom)
            r[:,1] = H1[1] + (H2[1] - H1[1])*D*(1.0 + num/denom)
            x[i,:] = r[:,0]
            y[i,:] = r[:,1]

        return x,y

class XY:
    """
    Clustering in both the x and y directions in a rectangular domain
    x1,x2: extension of the domain in the x direction
    y1,y2: extension of the domain in the y direction

    For meaning and range of D and beta see class INTERIOR

    DX: coordinate where clustering is desired is the x direction.
    DY: coordinate where clustering is desired is the y direction.  
    betax: a parameter that controls the clustering in the x direction
    betay: a parameter that controls the clustering in the y direction
    """
    def __init__(self,x1,x2,y1,y2):
        self.x1,self.x2,self.y1,self.y2 = x1,x2,y1,y2

    def __call__(self,N,M,DX=0.2,betax=6.5,DY=0.4,betay=5.5):
        x1,x2,y1,y2 = self.x1,self.x2,self.y1,self.y2

        epsilon = np.linspace(0,N-1,N)
        eta = np.linspace(0,M-1,M)
        x = np.zeros((N,M))
        y = np.zeros((N,M))      

        # ------------------------------------------------
        #       clustering in the x-direction
        # ------------------------------------------------
        
        # define the p_top and p_bottom arrays
        # notice: 'bottom' surface is at y = y1; 'top' surface is at y = y2
        p_top = np.zeros((N,2))
        p_bottom = np.zeros((N,2))

        H1 = x1
        H2 = x2

        for i in range(N):
            Anum = 1.0 + (np.exp(betax) - 1.0)*DX
            Adenom = 1.0 + (np.exp(-betax) - 1.0)*DX
            A = (0.5/betax)*np.log(Anum/Adenom)
            num = np.sinh(betax*(epsilon/(N-1) - A))
            denom = np.sinh(betax*A)
            p_bottom[:,0] = H1 + (H2 - H1)*DX*(1.0 + num/denom)
            p_bottom[:,1] = y1*np.ones(N)

        p_top[:,0] = p_bottom[:,0]
        p_top[:,1] = y2*np.ones(N)        

        # ------------------------------------------------
        #       clustering in the y-direction
        # ------------------------------------------------

        r = np.zeros((M,2))
        
        for i in range(N):
            H1 = p_bottom[i]
            H2 = p_top[i]
            Anum = 1.0 + (np.exp(betay) - 1.0)*DY
            Adenom = 1.0 + (np.exp(-betay) - 1.0)*DY
            A = (0.5/betay)*np.log(Anum/Adenom)
            num = np.sinh(betay*(eta/(M-1) - A))
            denom = np.sinh(betay*A)
            r[:,0] = H1[0] + (H2[0] - H1[0])*DY*(1.0 + num/denom)
            r[:,1] = H1[1] + (H2[1] - H1[1])*DY*(1.0 + num/denom)
            x[i,:] = r[:,0]
            y[i,:] = r[:,1]   
        return x,y

