function [t, y, nshots] = nonlinshoot(a, alpha, b, beta, fstring, fystring, fypstring,  tol, hstep, mk)
%M-file for EFR 10.7.
%This program will apply the non linear shooting method to 
%solve the BVP:  y''(t)=f(t,y,y'), y(a) = alpha, y(b) = beta
%Input variables:  a = left endpoint, alpha = left boundary value
%b = right endpoint, beta = right boundary value, fscript = inhomogenity
%function, inputted as a scipt (in sigle quotes) with variables t, y and yp (y'),
%the next two input variables are the partial derivatives of f(t, y, y')
%with respect to y and y' respectively, also inputted as scripts in
%the same fashion.  
%tol = tolerance, a positive number.  When successive approximations
%differ by less than tol at right endpoint, iterations stop.
%hstep = the step size to use in the Runge-Kutta method
%m0 = initial (shooting) slope; if this variable is not inputted 
%the default is m0 = (beta-alpha)/(b-a) is used
%Output variables:  t and y, two same sized vectors containing the
%time values and corresponding values of the numerical solution of the
%BVP, nshots, the number of iterations (shots) that were used in 
%the nonlinear shooting method.
%This program internally will call on Program 9.2:  'rksys'

%set default if necessary
if nargin < 10
   mk = (beta-alpha)/(b-a);
end

%set up a vector-valued inline function for the 4 equation linear system 
%that needs to be iteratively solved:
%Dy = yp,   y(a)=alpha,  Dyp = f(t,y,yp), yp(a) = mk
%Dz = zp,   z(a)=0, Dzp = zfy(t,y,yp) + zpfyp(t,y,yp), zp(a) = 1
%fvec will have 4 components [Dy Dyp Dz Dzp] and will be an inline
%function of the 2 variables t (a number) and vec (a vector)
% representing the four numbers y, yp, z, and zp in this order:
%vec(1) = y, vec(2) = yp, vec(3) = z, and vec(4) = zp
%we first change the inputted strings to conform to these new variables:
fstring=strrep(fstring,'yp','vec(2)');, fstring=strrep(fstring,'y','vec(1)');, 
fystring=strrep(fystring,'yp','vec(2)');, fystring=strrep(fystring,'y','vec(1)');
fypstring=strrep(fypstring,'y','vec(1)');, fypstring=strrep(fypstring,'yp','vec(2)');
fvec = inline(['[vec(2)  ', fstring, '  vec(4)  ',  'vec(3)*(', fystring, ')+vec(4)*(', fypstring, ')]'], ...
    't', 'vec');
%Note: Some of the blank spaces left above were intentional and important
%to separate the four components of this vector valued function.

%start iterative loop
nshots =1;
while 1
    [t, X] =rksys(fvec,a, b, [alpha  mk   0   1], hstep);
    y = X(:,1);, z=X(:,3);  %peel off the vectors we need
    Diff=y(length(y))-beta; 
    if abs(Diff)<tol
    return
    end
    mk=mk-Diff/z(length(z));, nshots= nshots+1; %update slope and shot counter
end


