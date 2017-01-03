function [xo,fo]=sim_anl(f,x0,l,u,kmax,q,TolFun,Kg)
% simulated annealing method to minimize f(x) s.t. l<=x<=u
N= length(x0);
x=x0; fx= feval(f,x);
xo=x; fo=fx;
if nargin<8, Kg=0; end %# of iterations to plot the graph
if nargin<7, TolFun=1e-8; end
if nargin<6, q=1; end %quenching factor
if nargin<5, kmax=100; end %maximum iteration number
for k=0:kmax
Ti=(k/kmax)^q; %inverse of temperature from 0 to 1
   mu=10^(Ti*100); % Eq.(7.1-23)
   dx= mu_inv(2*rand(size(x))-1,mu).*(u-l);
   x1=x+dx; %next guess
x1=(x1<l).*l +(l<=x1).*(x1<=u).*x1 +(u<x1).*u; 
%confine it inside the admissible region bounded by l and u.
   fx1=feval(f,x1); df=fx1-fx;
   if df<0|rand<exp(-Ti*df/(abs(fx)+eps)/TolFun) Eq.(7.1-24)
x=x1; fx=fx1;
   end
   if fx<fo, xo=x; fo=fx1; end
end
