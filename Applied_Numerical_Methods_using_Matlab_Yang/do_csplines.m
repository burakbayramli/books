%do_csplines.m
clear
KC=1; dy0=2; dyN=2; % with specified 1st derivatives 
x=[0 0.5  2  3.5  4]; y=[0  2 -2  2  0];
x=[0  1  2  3];  y=[0  1  4  5];
Ni=100; xi =x(1)+[0:Ni]*(x(end)-x(1))/Ni; %intermediate points
[yi,S]=cspline(x,y,xi,KC,dy0,dyN); %cubic spline interpolation
S %cubic spline coefficients
figure(1), clf, plot(x,y,'ko',xi,yi,'k:')
yi=spline(x,y,xi); %for comparison
hold on, pause, plot(x,y,'ro',xi,yi,'r')
yi=interp1(x,y,xi,'spline'); %for comparison
pause, plot(x,y,'bo',xi,yi,'b')
KC=3; [yi,S]=cspline(x,y,xi,KC); %with the 2nd derivatives extrapolated
%pp=mkpp(x,S); %make piecewise polynomial
%yi=ppval(pp,xi); %values of piecewise polynomial ftn
pause, plot(x,y,'ko',xi,yi,'k')
want_to_try_like_Hermite=0;
if want_to_try_like_Hermite==1
  x=[1  4];  y=[0  4];
  xi =x(1)+[0:Ni]*(x(end)-x(1))/Ni; %intermediate points
  [yi,S]=cspline(x,y,xi,1,0,1000); %cubic spline interpolation
  S
  pause, figure(2), clf
  plot(x,y,'ko',xi,yi,'k:')
end