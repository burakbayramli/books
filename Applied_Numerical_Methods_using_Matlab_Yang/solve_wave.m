%solve_wave
a=1; 
it0=inline('x.*(1-x)','x'); i1t0=inline('0'); %(E9.4-2a)
bx0t=inline('0');  bxft=inline('0'); %(E9.4-2b)
xf=1; M=20;  T=2; N=50; 
[u,x,t]=wave(a,xf,T,it0,i1t0,bx0t,bxft,M,N);
figure(1), clf
mesh(t,x,u)
figure(2), clf
for n=1:N %dynamic picture
  plot(x,u(:,n)), axis([0 xf -0.3 0.3]), pause(0.2)
end
