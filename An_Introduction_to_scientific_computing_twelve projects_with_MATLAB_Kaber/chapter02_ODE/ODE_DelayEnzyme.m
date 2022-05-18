%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function G=ODE_DelayEnzyme(t,Y,h,y0)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  function G=ODE_DelayEnzyme(t,Y,h)
%%  Exercise 2.6
%%  Right hand side function for the delayed differential system 
%%       G:[0,+infty[xR^4xR^4 -> R^4
%%  Input parameters:
%%        t  : time 
%%        Y  : array holding solution values up to time t
%%            (Y(:,1) holds initial  condition  therefore
%%             Y(:,i) holds solution at time (i-1)h
%%        h  : time step
%%       y0  : initial condition
%%  Output parameters:
%%        G  : value of the right hand side function
%%  Remark : y0 must be sent in input beside  Y because in the  
%%  delayed Runge Kutta scheme, the solution at intermediate time steps 
%%  g(:,1,:) is different from the initial solution y0.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global I
global td
delay4=round(td/h);
n=round(t/h)+1;
y=Y(:,n);
if n<delay4+1
  z=1/(1+0.0005*y0(4)^3);
else
  z=1/(1+0.0005*Y(4,n-delay4)^3);  
end
G=[I-z*y(1);...
    z*y(1)-y(2);...
    y(2)-y(3);...
    y(3)-0.5*y(4)];

 

