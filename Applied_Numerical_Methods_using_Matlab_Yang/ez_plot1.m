function ez_plot1(ftn,bounds,p)
if nargin<2, bounds=[-1 1]; end
b1=bounds(1); b2=bounds(2);
t=b1+[0:100]/100*(b2-b1);
if nargin<=2, x=feval(ftn,t);
 else  x=feval(ftn,t,p);
end
plot(t,x) 
  