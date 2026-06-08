import numpy as np
import matplotlib.pyplot as plt
from numpy import isnan

Re = 400 #400
Ns = [16, 32, 64]
N_ana = 128
domain = 1

def initializeU(gSize):
    
    u = np.zeros((gSize,gSize))
    u[-1,:] = 1 # U = 1
    
    return u

def initializeV(gSize):
    
    return np.zeros((gSize, gSize))


# Restriction function
def restriction(fine, gridSize, Initialize):
    
    gSize = int(gridSize/2)
    xs = np.linspace(0, domain, gSize)
    ys = np.linspace(0, domain, gSize)
    x, y = np.meshgrid(xs, ys)

    # Implement boundary conditions
    coarse = Initialize(gSize)
    
    # 2D averaging for the rest of the points
    # Iterate through the coarse mesh size
    for c_j in range(1, gSize-1):
        
        for c_i in range(1, gSize-1):
            
            k_j = int(2*c_j)
            k_i = int(2*c_i)
            
            #print(c_j, c_i)
            n_avg = 8
            
            a1 = fine[k_j - 1, k_i - 1]
            a2 = fine[k_j - 1, k_i]
            a3 = fine[k_j - 1, k_i + 1]
            a4 = fine[k_j + 1, k_i - 1]
            a5 = fine[k_j + 1, k_i] 
            a6 = fine[k_j + 1, k_i + 1]
            a7 = fine[k_j, k_i - 1]
            a8 = fine[k_j, k_i + 1]

            coarse[c_j, c_i] = ( a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8) / n_avg
            
    return coarse

"""
Input: arrays or lists of independent and dependent variables (same length)
Output: tuple of linear fit coefficients (m = slope, b = y-intercept)
"""
def linearFit(xs, ys):
    
    mean_xs = sum(xs)/len(xs)
    mean_ys = sum(ys)/len(ys)
    
    m_numerator = 0
    m_denominator = 0
    
    for i in range(len(xs)):
        
        m_numerator += (xs[i] - mean_xs)*(ys[i] - mean_ys)
        m_denominator += (xs[i] - mean_xs)**2
    
    m = m_numerator / m_denominator
    b = mean_ys - (m * mean_xs)
    
    return (m, b)

# numerical
num_64_u = np.loadtxt("Re=" + str(Re) + "_N=64_u.txt", dtype=float)
num_32_u = np.loadtxt("Re=" + str(Re) + "_N=32_u.txt", dtype=float)
num_16_u = np.loadtxt("Re=" + str(Re) + "_N=16_u.txt", dtype=float)
num_64_v = np.loadtxt("Re=" + str(Re) + "_N=64_v.txt", dtype=float)
num_32_v = np.loadtxt("Re=" + str(Re) + "_N=32_v.txt", dtype=float)
num_16_v = np.loadtxt("Re=" + str(Re) + "_N=16_v.txt", dtype=float)

# analytical
ana_u = np.loadtxt("Re=" + str(Re) + "_N=128_u.txt", dtype=float)
ana_v = np.loadtxt("Re=" + str(Re) + "_N=128_v.txt",dtype=float)

# create analytical soln for N = 64
ana_64_u = restriction(ana_u, 128, initializeU)
ana_64_v = restriction(ana_v, 128, initializeV)

# create analytical soln for N = 32
ana_32_u = restriction(ana_64_u, 64, initializeU)
ana_32_v = restriction(ana_64_v, 64, initializeV)

# create analytical soln for N = 16
ana_16_u = restriction(ana_32_u, 32, initializeU)
ana_16_v = restriction(ana_32_v, 32, initializeV)


errors = list()

errors.append( np.sqrt(np.sum(np.sum((ana_16_u - num_16_u)**2, axis=0)))/14**2 )
errors.append( np.sqrt(np.sum(np.sum((ana_32_u - num_32_u)**2, axis=0)))/30**2 )
errors.append( np.sqrt(np.sum(np.sum((ana_64_u - num_64_u)**2, axis=0)))/62**2 )

xvals = np.log(Ns)
yvals = np.log(errors)
m, b = linearFit(xvals, yvals)
print(m,b)
a1 = 2.7
a2 = 4.2
xs = np.linspace(a1, a2, 100)
ys = m*xs + b


plt.figure(dpi=800)
plt.plot(xs, ys, 'k', label = "Best Fit")
plt.plot(xvals, yvals, 'r--o', label = "Results")
plt.xlim(a1, a2)
plt.legend()
plt.suptitle(r"Log-log for Spatial Convergence in $u$")
plt.title(f"y = {round(m,2)}x + {round(b,2)}")
plt.xlabel("x = Log(N)")
plt.ylabel("y = Log(L2-norm)")
plt.show()

errors = list()

errors.append( np.sqrt(np.sum(np.sum((ana_16_v - num_16_v)**2, axis=0)))/14**2 )
errors.append( np.sqrt(np.sum(np.sum((ana_32_v - num_32_v)**2, axis=0)))/30**2 )
errors.append( np.sqrt(np.sum(np.sum((ana_64_v - num_64_v)**2, axis=0)))/62**2 )

xvals = np.log(Ns)
yvals = np.log(errors)
m, b = linearFit(xvals, yvals)
print(m,b)
a1 = 2.7
a2 = 4.2
xs = np.linspace(a1, a2, 100)
ys = m*xs + b


plt.figure(dpi=800)
plt.plot(xs, ys, 'k', label = "Best Fit")
plt.plot(xvals, yvals, 'r--o', label = "Results")
plt.xlim(a1, a2)
plt.legend()
plt.suptitle(r"Log-log for Spatial Convergence in $v$")
plt.title(f"y = {round(m,2)}x + {round(b,2)}")
plt.xlabel("x = Log(N)")
plt.ylabel("y = Log(L2-norm)")
plt.show()



            
            
            
            
            
            
            
            
            
            