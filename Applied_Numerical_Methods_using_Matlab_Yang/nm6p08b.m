%nm6p08b.m: finite difference method
a1=inline('-2*t./(t.^2+1)','t');  a0=inline('2./(t.^2+1)','t');
u=inline('t.^2+1','t'); 
t0=0; tf=1; N=500;    
c0=[1 6 0]; cf=[1 1 0];%coefficient vectors of boundary condition 
[tt,x_fd]=bvp2mm_fdf(a1,a0,u,t0,tf,c0,cf,N);
plot(tt,x_fd,'r')
