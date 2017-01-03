%nm5p01
f= inline('x.*(x.*x-2)', 'x');
n=[1 -1];  x0=1;  h= 0.1;  DT=1;
c= difapx(1,n);  i=1:length(c);
num= c*feval(f,x0+(n(1)+1-i)*h)'; drv=num/h;
fprintf('with h=%6.4f, %12.6f %12.4e\n', h,drv,drv-DT);
