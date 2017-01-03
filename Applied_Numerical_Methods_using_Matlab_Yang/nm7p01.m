%nm7p01.m  to perform the revised golden search method
f701=inline('x.*(x-2)', 'x');
a=0; b=3; r=(sqrt(5)-1)/2; 
TolX=1e-4; TolFun=1e-4; MaxIter=100; 
h=b-a; rh= r*h; 
c=b-rh; d=a+rh;
fc=f701(c);  fd=f701(d); 
if fc<fd, [xo,fo]=opt_gs1(f701,a,c,fc,1-r,d,r,TolX,TolFun,MaxIter)
 else [xo,fo]=opt_gs1(f701,c,d,fd,r,b,r, TolX,TolFun,MaxIter)
end
