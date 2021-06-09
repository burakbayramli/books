function tablenNG(w,h,b,t0)
% tablenNG  Use Newton's method to find dimensions of picnic table legs
%           Variable input arguments are passed through to newtonNG
%
% Synopsis:  tablenNG(w,b,h)
%            tablenNG(w,b,h,t0)
%
% Input:     w,h = width and height of the table legs
%            b   = width of the material used to make the legs
%            t0  = (optional) initial guess at theta.  Default: t0 = pi/4 (rad)
%
% Output:    Print out table dimensions

if nargin<3
   error('All three dimensions, w, h, b, must be specified');
elseif nargin<4
   t0 = pi/4;        % default initial guess
end

theta = newtonNG('legsnNG',t0,[],[],0,w,h,b);      %  Root-finding is done here

%  --- Compute other dimensions once theta is known
alpha = pi/2 - theta;     a  = b/tan(alpha);      c = b/tan(theta);
d2 = h/(2*sin(theta));    d1 = d2 - a - c;  
fprintf('theta = %6.1f degrees   d1 = %8.3f   d2 = %8.3f\n',theta*180/pi,d1,d2);
