function damper(xm,y1,y2,w,color)
%draws a damper in (xm-0.5 xm+0.5 y1 y2)
if nargin<5, color='k'; end
ym=(y1+y2)/2;
xD1= xm+w*[0.3*[0  0  -1  1]];
yD1= [y2+w  ym  ym  ym];
xD2= xm+w*[0.5*[-1 -1  1  1]];
yD2= ym+w*[1  -1  -1  1];
xD3= xm+[0  0];
yD3= [y1  ym]-w;
plot(xD1,yD1,color, xD2,yD2,color, xD3,yD3,color)
