function spring(n,p1,p2,w,color)
%draw a spring of n windings, width w from p1 to p2
if nargin<5, color='k'; end
c= (p2(1)-p1(1))/2; d= (p2(2)-p1(2))/2; 
f= (p2(1)+p1(1))/2; g= (p2(2)+p1(2))/2; 
y= -1:0.01:1;  t= (y+1)*pi*(n+0.5);
x=-0.5*w*sin(t); y= y+0.15*(1-cos(t)); 
a=y(1); b=y(length(x));
y= 2*(y-a)/(b-a)-1;
yyS= d*y - c*x +g;
xxS= x +f;
xxS1= [f  f];
yyS1= yyS(length(yyS))+[0 w];
yyS2= yyS(1)-[0 w];
plot(xxS,yyS,color, xxS1,yyS1,color, xxS1,yyS2,color)




