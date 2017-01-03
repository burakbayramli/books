function tablez(w,h,b,t0)
% tablez  Use fzero to find dimensions of picnic table legs
%
% Synopsis:  tablez(w,b,h)
%            tablez(w,b,h,t0)
%
% Input:     w = width of the table legs
%            h = height of the table legs
%            b = width of the stock used to make the legs
%            t0 = (optional) initial guess at theta (in radians)
%                 Default:  t0 = pi/4
%
% Output:    Print out of table dimensions

if nargin<3
   error('All three dimensions, w, h, b, must be specified');
elseif nargin<4
   t0 = pi/4;        % default initial guess
end

[v,d] = version;
vnum = str2num(v(1:3));   % vnum is version number in d.d format
if vnum<5.3
  % "old" fzero syntax
  theta = fzero('legz',t0,[],0,w,h,b);   %  w,h,b are passed through to legz
else
  % optimset('Display','off') stops printing *and* avoids warning from parser
  theta = fzero('legz',t0,optimset('Display','off'),w,h,b);
end

%  --- Compute other dimensions once theta is known
alpha = pi/2 - theta;     a  = b/tan(alpha);      c = b/tan(theta);
d2 = h/(2*sin(theta));    d1 = d2 - a - c;  
fprintf('theta = %6.1f degrees   d1 = %8.3f   d2 = %8.3f\n',theta*180/pi,d1,d2);
