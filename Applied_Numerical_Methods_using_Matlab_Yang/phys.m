function f=phys(x,scale)
if nargin<2, scale=1; end
global G Ms Me R T
w=2*pi/T; x=x*scale; f=G*(Ms./(x.^2+eps)-Me./((R-x).^2+eps))-x.*w^2;