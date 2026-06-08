# -*- coding: utf-8 -*-
"""
Created on Fri Nov  5 11:56:17 2021

@author: asus
"""
import numpy as np
import matplotlib.pyplot as plt

#Re_vals = [100, 400]
N_vals = [16, 32, 64, 128]
N_vals400 = [16, 32, 64, 128, 256]

# u_min, v_min, v_max are ROWS
# 32, 64, 128 are columns
vals_100 = np.zeros((3,3))

x_arr = np.linspace(10,260,100)
y_arr = np.ones(100)

"""
Re = 100
"""
u_min_100 = [-0.17780861045947072, -0.18596731081182707, -0.19368499110783324, -0.1976011613209984]
v_min_100 = [-0.19635562348048888, -0.20521791633797937, -0.21602446852290258, -0.22148543943943744]
v_max_100 = [0.1517513022971393, 0.1556582789738809, 0.1603553820549218, 0.16260749611459957]

u_min_100_actual = -0.21090
v_min_100_actual = -0.24533
v_max_100_actual = 0.17527

"""
Re = 400
"""
u_min_400 = [-0.29788146865090903, -0.2745385820270402, -0.28917299219395515, -0.3048874222993818, -0.31241887797094]
v_min_400 = [-0.3777462103251244, -0.37130327044508976, -0.3930351983546282, -0.41425875952457514, -0.426592481584274]
v_max_400 = [0.2552130969588724, 0.2462630819164556, 0.26102500666570266, 0.2764356738534338, 0.28573766821727425]

u_min_400_actual = -0.32726
v_min_400_actual = -0.44993
v_max_400_actual = 0.30203

"""
u_min plot
"""
plt.figure(dpi=800)
plt.plot(N_vals, u_min_100, 'g--.', label = "Re = 100")
plt.plot(x_arr, y_arr*u_min_100_actual, 'k')
plt.plot(N_vals400, u_min_400, 'm--.', label = "Re = 400")
plt.plot(x_arr, y_arr*u_min_400_actual, 'k', label = "Actual")
plt.xlim(10, 260)
plt.title(r"$u_{min}$")
plt.legend()
plt.xticks(N_vals400)
plt.xlabel("N")
plt.ylabel("Velocity at vertical centerline")
plt.show()

"""
v_min plot
"""
plt.figure(dpi=800)
plt.plot(N_vals, v_min_100, 'g--.', label = "Re = 100")
plt.plot(x_arr, y_arr*v_min_100_actual, 'k')
plt.plot(N_vals400, v_min_400, 'm--.', label = "Re = 400")
plt.plot(x_arr, y_arr*v_min_400_actual, 'k', label = "Actual")
plt.xlim(10, 260)
plt.title(r"$v_{min}$")
plt.legend()
plt.xticks(N_vals400)
plt.xlabel("N")
plt.ylabel("Velocity at horizontal centerline")
plt.show()

"""
v_max plot
"""
plt.figure(dpi=800)
plt.plot(N_vals, v_max_100, 'g--.', label = "Re = 100")
plt.plot(x_arr, y_arr*v_max_100_actual, 'k')
plt.plot(N_vals400, v_max_400, 'm--.', label = "Re = 400")
plt.plot(x_arr, y_arr*v_max_400_actual, 'k', label = "Actual")
plt.xlim(10, 260)
plt.title(r"$v_{max}$")
plt.legend()
plt.xticks(N_vals400)
plt.xlabel("N")
plt.ylabel("Velocity at horizontal centerline")
plt.show()








