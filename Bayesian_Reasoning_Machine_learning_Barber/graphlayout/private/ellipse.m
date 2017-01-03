function [p] = ellipse(x, y, rx, ry, c)
% ELLIPSE		Draws Ellipse shaped patch objects
% 
%  [<P>] = ELLIPSE(X, Y, Rx, Ry, C)
% 
% Inputs :
%    X : N x 1 vector of x coordinates
%    Y : N x 1 vector of y coordinates
%    Rx, Ry : Radii
%    C : Color index
%
% 
% Outputs :
%    P = Handles of Ellipse shaped path objects
% 
% Usage Example : [] = ellipse();
% 
% 
% Note     :
% See also 

% Uses :

% Change History :
% Date		Time		Prog	Note
% 27-May-1998	 9:55 AM	ATC	Created under MATLAB 5.1.0.421

% ATC = Ali Taylan Cemgil,
% SNN - University of Nijmegen, Department of Medical Physics and Biophysics
% e-mail : cemgil@mbfys.kun.nl 

if (nargin < 2) error('Usage Example : e = ellipse([0 1],[0 -1],[1 0.5],[2 0.5]); '); end;
if (nargin < 3) rx = 0.1; end;
if (nargin < 4) ry = rx; end;
if (nargin < 5) c = 1; end;

if length(c)==1, c = ones(size(x)).*c; end;
if length(rx)==1, rx = ones(size(x)).*rx; end;
if length(ry)==1, ry = ones(size(x)).*ry; end;
  
n = length(x);
p = zeros(size(x));
t = 0:pi/30:2*pi;
for i=1:n,
	px = rx(i)*cos(t)+x(i);
	py = ry(i)*sin(t)+y(i);
	p(i) = patch(px,py,c(i));
end;

if nargout>0, pp = p; end;