""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    
# rk4.py: rk4 algorithm computes Delta y, input y & f; do NOT modify

def rk4Algor(t, h, N, y, f):

    k1 = [0]*(N); k2 = [0]*(N);  k3 = [0]*(N);  k4 = [0]*(N) 
    fvector = [0]*(N)
    ydumb = [0]*(N)
    fvector = f(t, y)                     # Returns RHS's  
    for i in range(0, N):  k1[i] = h*fvector[i]                             
    for i in range(0, N):  ydumb[i] = y[i] + k1[i]/2. 
    fvector = f(t+h/2., ydumb)
    for i in range(0, N):  k2[i] = h*fvector[i]
    for i in range(0, N):  ydumb[i] = y[i] + k2[i]/2.
    fvector = f(t+h/2., ydumb)
    for i in range(0, N):  k3[i] = h*fvector[i]
    for i in range(0, N):  ydumb[i] = y[i] + k3[i] 
    fvector = f(t+h, ydumb)
    for i in range(0, N):  k4[i] = h*fvector[i]
    for i in range(0, N):  y[i] = y[i] + (k1[i]+2.*(k2[i]+k3[i])+k4[i])/6.
    return y    
