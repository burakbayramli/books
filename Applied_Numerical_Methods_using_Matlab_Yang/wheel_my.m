function wheel_my(xm,ym,w,color)
%draws a wheel of size w at center (xm,ym)
if nargin<5, color='k'; end
xW1= xm+w*1.2*[-1  1];
yW1= ym+w*[2  2];
xW2= xm*[1  1];
yW2= ym+w*[2  0];
plot(xW1,yW1,color, xW2,yW2,color)
th=[0:100]/50*pi;
plot(xm+j*ym+w*exp(j*th),color)
