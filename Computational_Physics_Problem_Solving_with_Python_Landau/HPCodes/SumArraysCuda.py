""" From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation"""
    
# SumArraysCuda.py: sums arrays a + b = c

import pycuda.autoinit
import pycuda.driver as drv
import numpy
from pycuda.compiler import SourceModule 

                       # The kernel in C  
mod = SourceModule("""
__global__ void sum_ab(float *c, float *a, float *b)
{ const int i = threadIdx.x;
  c[i] = a[i] + b[i]; }
                   """)
sum_ab = mod.get_function("sum_ab")
N = 32
a = numpy.array(range(N)).astype(numpy.float32)  
b = numpy.array(range(N)).astype(numpy.float32)  
for i in range (0, N):
  a[i] = 2.0*i                    
  b[i] = 1.0*i                    
c = numpy.zeros_like(a)               # intialize c
a_dev = drv.mem_alloc(a.nbytes)       # reserve memory in device
b_dev = drv.mem_alloc(b.nbytes)        
c_dev = drv.mem_alloc(c.nbytes)        
drv.memcpy_htod(a_dev, a)               # copy a to device
drv.memcpy_htod(b_dev, b)               # copy b to device
sum_ab( c_dev, a_dev, b_dev, block=(32,1,1), grid=(1,1))   
print("a" + \n + a + \n + "b"+ \n + b)                             
drv.memcpy_dtoh(c,c_dev)              # copy c from device
print("c" +\n + c)                               