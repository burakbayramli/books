function [t, y] = rkf45(varf, a, b, y0, tol, hinit, hmin, hmax)
% input variables:  varf, a, b, y0, tol, hinit, hmin, hmax
% output variables:  t, y
% varf is a function of two variables varf(t,y).  The program will
% apply Runge-Kutta to solve the IVP:  (DE):  y'=varf(t,y), (IC)
% y(a)=y0 on the t-interval [a,b] with step size hstep.  The output
% will be a vector of t's and corresponding y's
% the last four input variables are optional and are as follows:
% tol = the target goal for the global error, default = 1e-5
% hinit = initial step size, default = 0.1
% hmin = minimum allowable step size, default = 1e-5
% hmax = maximum allowable step size, default = 1
% program will terminate with an error flag if it is necessary to 
% use a step size smaller than hmin

%set default input variables as needed
if nargin<5, tol=1e-5;, end
if nargin<6, hinit=0.1;, end
if nargin<7, hmin=1e-5;, end
if nargin<8, hmax=1;, end

t(1)=a;, y(1)=y0;, n=1;
h=hinit; 
flag =0; %this flag will keep track if maximum step size has been reached.
flag2 =0; %this flag will keep track if minimum step size has been reached.
while  t(n)<b
   k1=h*feval(varf,t(n),y(n));
   k2=h*feval(varf,t(n)+h/4,y(n)+k1/4);
   k3=h*feval(varf,t(n)+3*h/8,y(n)+3*k1/32+9*k2/32);
   k4=h*feval(varf,t(n)+12*h/13,y(n)+(1932*k1-7200*k2+7296*k3)/2197);
   k5=h*feval(varf,t(n)+h,y(n)+439*k1/216-8*k2+3680*k3/513-845*k4/4104);
   k6=h*feval(varf,t(n)+h/2,y(n)-8*k1/27+2*k2-3544*k3/2565+1859*k4/4104-11*k5/40);
   E=abs(k1/360-128*k3/4275-2197*k4/75240+k5/50+2*k6/55);
   
   if E > h*tol  %step size is too large, reduce to half and try again
     hnew=h/2;
     if hnew<hmin %minimum step size has been reached, accept approximation but set warning flag2
         flag2=1;
         t(n+1)=t(n)+h;
         y(n+1)=y(n)+16*k1/135+6656*k3/12825+28561*k4/56430-9*k5/50+2*k6/55;
         n=n+1;
         h=hmin;
     else
         h=hnew;
     end
   elseif E < h*tol/4 %step size is too small, accept approximation but double next step size
     t(n+1)=t(n)+h;
     y(n+1)=y(n)+16*k1/135+6656*k3/12825+28561*k4/56430-9*k5/50+2*k6/55;
     n=n+1;
     h=2*h;
     if h>=hmax
         flag=1;
         h=hmax;
     end
   else %accept approximation and proceed
     t(n+1)=t(n)+h;
     y(n+1)=y(n)+16*k1/135+6656*k3/12825+28561*k4/56430-9*k5/50+2*k6/55;
     n=n+1;
 end
end
if flag ==1
     fprintf('In the course of the RKF45 program, the maximum step size has been reached.')
end
if flag2 ==1
     fprintf('WARNING:  Minimum step size has been reached; it is recommended to run the \r')
     fprintf('program again with a smaller hmin and or a larger tol')
end
   