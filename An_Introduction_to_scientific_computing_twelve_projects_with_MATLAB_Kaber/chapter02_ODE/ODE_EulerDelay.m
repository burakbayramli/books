%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [T,Y]=ODE_EulerDelay(fdelay,tmax,nmax,y0)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  function [T,Y]=ODE_EulerDelay(fdelay,tmax,nmax,y0)
%%  Exercise 2.6
%%  Function ODE_EulerDelay to integrate the system with delay term
%%  U'(t)=fdelay(t,U,V), U,V:R->R^4,   
%%       fdelay:[0,+infty[xR^4xR^4 -> R^4
%%  Input parameters:
%%        fdelay : right hand side function
%%        tmax    : final time
%%        nmax    : number of time steps
%%        y0      : initial condition
%%  Output parameters:
%%        T       : array holding discrete times 
%%        Y       : array holding the  solution at times T
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
h=tmax/nmax;
Y=zeros(length(y0),nmax+1);
T=zeros(1,nmax+1);
Y(:,1)=y0;
for i=1:nmax
  t=(i-1)*h;  
  Y(:,i+1)= Y(:,i)+h*feval(fdelay,t,Y,h,y0);
  T(i+1)=t;
end

