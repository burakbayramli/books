function f=phys1(x,scale)
if nargin<2, scale=1; end
global G Ms Me R T
w=2*pi/T; x=x*scale; f=(R-x).^2.*(w^2*x.^3-G*Ms)+G*Me*x.^2;