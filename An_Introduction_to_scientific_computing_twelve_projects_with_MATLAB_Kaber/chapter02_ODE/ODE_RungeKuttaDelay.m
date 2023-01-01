%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [T,Y]=ODE_RungeKuttaDelay(fdelay,tmax,nmax,y0)
%%  Exercise 2.6
%%  Function ODE_RungeKuttaDelay to integrate the system with delay term
%%  U'(t)=fdelay(t,U,V), U,V:R->R^4,   
%%       fdelay:[0,+infty[xR^4xR^4 -> R^4
%%  Input parameters:
%%        fdelay  : right hand side function
%%        tmax    : final time
%%        nmax    : number of time steps
%%        y0      : initial condition
%%  Output parameters:
%%        T       : array holding discrete times 
%%        Y       : array holding the  solution at times T
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
h=tmax/nmax;
Y=zeros(length(y0),nmax+1);
g=zeros(length(y0),nmax+1,4);
for i=1:4 
    g(:,1,i)= y0;
end
T=zeros(1,nmax+1);
Y(:,1)=y0;
b=[1,2,2,1]/6;
for n=1:nmax
  t=(n-1)*h;  
  g(:,n,1)= Y(:,n);
  g(:,n,2)= Y(:,n)+h*feval(fdelay,t,g(:,:,1),h,y0)/2;
  g(:,n,3)= Y(:,n)+h*feval(fdelay,t,g(:,:,2),h,y0)/2;
  g(:,n,4)= Y(:,n)+h*feval(fdelay,t,g(:,:,3),h,y0);
  Y(:,n+1)= Y(:,n);
  for j=1:4
    Y(:,n+1)=Y(:,n+1)+h*b(j)*feval(fdelay,t,g(:,:,j),h,y0);
  end
  T(n+1)=t;
end
