%nm119_8: example of while loops
x=1; k1=0;
while x/2>0
   x=x/2; k1=k1+1;
end
k1, x_min=x;
fprintf('x_min  is %20.18e=%bx\n',x_min,x_min);
x=1; k2=0;
while 2*x<inf
   x=x*2; k2=k2+1;
end
k2, x_max0=x;
tmp=1; k3=0;
while x_max0*(2-tmp/2)<inf
   tmp=tmp/2; k3=k3+1;
end   
k3, x_max=x_max0*(2-tmp);
fprintf('x_max=%20.18e=%bx\n',x_max,x_max);
%fprintf('x_min=%20.18e=%bx,\n',x_min,x_min);
fprintf('1  is %20.18e=%bx,\n',1,1);
format long e
x_min,-x_min,x_max,-x_max
format hex
x_min,-x_min,x_max,-x_max
format short
