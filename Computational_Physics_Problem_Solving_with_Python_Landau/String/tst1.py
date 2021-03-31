import numpy as np
import matplotlib.pyplot as plt

rho = 0.01;
ten = 40.;
c = np.sqrt(ten/rho)               
c1 = c;
ratio =  c*c/(c1*c1)  # CFL kriteri = 1
xi = np.zeros((101,3), float)                            
k = range(0,101)
 
         
for i in range(0, 81):
   xi[i, 0] = 0.00125*i         
for i in range (81, 101):
   xi[i, 0] = 0.1 - 0.005*(i - 80) 

fig = plt.figure()  
for i in range(1,100): 
    xi[i,1] = xi[i,0] + 0.5*ratio*(xi[i+1,0] + xi[i-1,0] -2*xi[i,0])  
    
for j in range(100):
   for i in range(1, 100):              
      xi[i,2] = 2.*xi[i,1]-xi[i,0]+ratio*(xi[i+1,1]+xi[i-1,1]-2*xi[i,1])
   if j % 5 == 0:
      plt.grid(True)                                                        
      plt.xlim(0, 101)
      plt.ylim(-0.15, 0.15)
      plt.plot(k,xi[k,2])
      plt.show()
   for m in range (0,101):                                
      xi[m, 0] = xi[m, 1]                                    
      xi[m, 1] = xi[m, 2]
      
