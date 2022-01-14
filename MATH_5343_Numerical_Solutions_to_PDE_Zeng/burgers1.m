N = 10
cfl = 0.01
x_l = 0.0
x_r = 1.0
T = 10
u_l = 0.0
u_r = 1.0
[x, u] = burgers_riem(N,cfl,x_l,x_r,T,u_l,u_r)
