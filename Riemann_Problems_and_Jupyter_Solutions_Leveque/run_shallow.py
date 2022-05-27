# 
# Burak Bayramli
# Runs Leveque example through simple script, no ipython dependencies
# Taken from Shallow_water_approximate.ipynb
import numpy as np
import matplotlib.pyplot as plt

def shallow_water_roe(q_l, q_r, grav=1.):
    h_l = q_l[0]
    hu_l = q_l[1]
    u_l = hu_l/h_l
    h_r = q_r[0]
    hu_r = q_r[1]
    u_r = hu_r/h_r
    
    delta = q_r - q_l
    
    # Roe averages
    h_hat = (h_r + h_l)/2.
    u_hat = (np.sqrt(h_r)*u_r + np.sqrt(h_l)*u_l) \
                / (np.sqrt(h_r) + np.sqrt(h_l))
    c_hat = np.sqrt(grav*h_hat)
    
    s1 = u_hat - c_hat
    s2 = u_hat + c_hat
    
    alpha1 = ( (u_hat+c_hat)*delta[0] - delta[1])/(2*c_hat)
    alpha2 = (-(u_hat-c_hat)*delta[0] + delta[1])/(2*c_hat)
    
    h_m = q_l[0] + alpha1
    hu_m = q_l[1] + alpha1 * (u_hat - c_hat)
    q_m = np.array([h_m, hu_m])
    
    states = np.column_stack([q_l,q_m,q_r])
    speeds = [s1, s2]
    wave_types = ['contact','contact']
    
    def reval(xi):
        h_out  = (xi<s1)*h_l + (s1<=xi)*(xi<=s2)*h_m + (s2<xi)*h_r
        hu_out = (xi<s1)*hu_l + (s1<=xi)*(xi<=s2)*hu_m + (s2<xi)*hu_r
        return h_out, hu_out
    
    return states, speeds, reval, wave_types

h_l = 4; h_r = 1
u_l = 0; u_r = 0
hu_l = h_l*u_l; hu_r = h_r*u_r
q_l = np.array([h_l, hu_l]); q_r = np.array([h_r, hu_r])
xmax = 2
x = np.linspace(-xmax, xmax, 1000)
states, speeds, reval, wave_types = shallow_water_roe(q_l, q_r)
t = 0.5 # change this value to see graph at different time points
q = reval(x/(t+1e-10))
print (q)
plt.plot(q[0])
plt.show()

