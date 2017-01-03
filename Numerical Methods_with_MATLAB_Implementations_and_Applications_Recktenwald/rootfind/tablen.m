function tablen(w,h,b,t0)
% tablen  Use Newton's method to find dimensions of picnic table legs
%
% Synopsis:  tablen(w,b,h)
%            tablen(w,b,h,t0)
%
% Input:     w,h = width and height of the table legs
%            b   = width of the material used to make the legs
%            t0  = (optional) initial guess at theta.  Default: t0 = pi/4 (rad)
%
% Output:    Print out table dimensions

global WLENGTH HLENGTH BLENGTH            %  Define global variables
WLENGTH = w;  HLENGTH = h;  BLENGTH = b;  %  Global copies of input variables

if nargin<3
   error('All three dimensions, w, h, b, must be specified');
elseif nargin<4
   t0 = pi/4;        % default initial guess
end

theta = newton('legsn',t0);      %  Root-finding is done in newton

%  --- Compute other dimensions once theta is known
alpha = pi/2 - theta;     a  = BLENGTH/tan(alpha);      c = BLENGTH/tan(theta);
d2 = HLENGTH/(2*sin(theta));    d1 = d2 - a - c;  
fprintf('theta = %6.1f degrees   d1 = %8.3f   d2 = %8.3f\n',theta*180/pi,d1,d2);
