# -*- coding: utf-8 -*-
"""
Created on Sat Dec 11 13:26:13 2021

@author: asus
"""
import numpy as np
import matplotlib.pyplot as plt

N = 51
Re = 1000
title_u = "Re=" + str(Re) + "_N=" + str(N) + "_u.txt"
title_v = "Re=" + str(Re) + "_N=" + str(N) + "_v.txt"

u = np.loadtxt(title_u)
v = np.loadtxt(title_v)  

xmin = 0
xmax = 1
ymin = 0
ymax = 1

xs = np.linspace(xmin, xmax, N)
ys = np.linspace(ymin, ymax, N)
x, y = np.meshgrid(xs, ys)


half = int(np.floor(N/2))
vertical = u[:,half]
horizontal = v[half,:]

if N%2 == 0:
    
    vertical = (u[:,half] + u[:,half-1])/2
    horizontal = (v[half,:] + v[half-1,:])/2
    
# vertical centerline at num half
#"""
plt.figure(dpi = 800)
plt.plot(ys, vertical, 'r')
plt.plot(ys, vertical, 'k.')
plt.title(r"$u$ along the the vertical centerline $x=0.5$")
plt.xlabel(r"$y$")
plt.ylabel(r"$x$-velocity: $u$")
plt.savefig('out2.jpg')

print(f"u min is {min(vertical)}")

plt.figure(dpi = 800)
plt.plot(xs, horizontal, 'r')
plt.plot(xs, horizontal, 'k.')
plt.title(r"$v$ along the the horizontal centerline $y=0.5$")
plt.xlabel(r"$x$")
plt.ylabel(r"$y$-velocity: $v$")
plt.savefig('out1.jpg')
#"""
print(f"v min is {min(horizontal)}")
print(f"v max is {max(horizontal)}")


print(f"\nu min is {min(vertical*2.5)}")
print(f"v min is {min(horizontal*2.5)}")
print(f"v max is {max(horizontal*2.5)}")
