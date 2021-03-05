""" From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation"""
    
# SumArraysCuda2.py: sums arrays a + b = c using a different block structure 
import pycuda.autoinit
import pycuda.driver as drv
import numpy
from pycuda.compiler import SourceModule

                         # kernel in C language
mod = SourceModule("""
__global__ void sum_ab(float *c, float *a, float *b)
{ const int i = threadIdx.x+blockDim.x*blockIdx.x;
  c[i] = a[i] + b[i];
}
""")

sum_ab = mod.get_function("sum_ab")
N=32
a = numpy.array(range(N)).astype(numpy.float32)
b = numpy.array(range(N)).astype(numpy.float32)
for i in range (0,N):
  a[i] = 2.0*i                   # assign a
  b[i] = 1.0*i                   # assign b
c = numpy.zeros_like(a)         # sum on device
sum_ab( drv.Out(c), drv.In(a), drv.In(b), block=(8,1,1), grid=(4,1) )       
print("a" + \n + a + \n+ "b" + \n + "c" + \n + c) 