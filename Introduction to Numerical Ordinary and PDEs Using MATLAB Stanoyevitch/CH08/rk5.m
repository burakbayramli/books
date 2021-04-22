function [t,y]=rk5(varf,a,b,y0,h)
t(1)=a;, y(1)=y0;
nmax=ceil((b-a)/h);
for n=1:nmax
   t(n+1)=t(n)+h;
   k1=h*feval(varf,t(n),y(n));
   k2=h*feval(varf,t(n)+h/4,y(n)+k1/4);
   k3=h*feval(varf,t(n)+3*h/8,y(n)+3*k1/32+9*k2/32);
   k4=h*feval(varf,t(n)+12*h/13,y(n)+(1932*k1-7200*k2+7296*k3)/2197);
   k5=h*feval(varf,t(n)+h,y(n)+439*k1/216-8*k2+3680*k3/513-845*k4/4104);
   k6=h*feval(varf,t(n)+h/2,y(n)-8*k1/27+2*k2-3544*k3/2565+1859*k4/4104-11*k5/40);
   y(n+1)=y(n)+16*k1/135+6656*k3/12825+28561*k4/56430-9*k5/50+2*k6/55;
end

   