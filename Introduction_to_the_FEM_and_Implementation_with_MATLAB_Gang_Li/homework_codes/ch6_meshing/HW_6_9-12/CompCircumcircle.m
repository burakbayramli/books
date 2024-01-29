function [x0,y0,r]=CompCircumcircle(a,b,c)
global nodes;
xa=nodes(a,1); ya=nodes(a,2); 
xb=nodes(b,1); yb=nodes(b,2); 
xc=nodes(c,1); yc=nodes(c,2); 
a=det([xa ya 1
       xb yb 1
       xc yc 1]);
bx=-det([xa^2 + ya^2 ya 1
         xb^2 + yb^2 yb 1
         xc^2 + yc^2 yc 1]);
by=det([xa^2 + ya^2 xa 1
        xb^2 + yb^2 xb 1
        xc^2 + yc^2 xc 1]);
c= -det([xa^2 + ya^2 xa ya
         xb^2 + yb^2 xb yb
         xc^2 + yc^2 xc yc]);  
x0=-bx/2/a;
y0=-by/2/a;
r=sqrt(bx^2 + by^2 - 4*a*c)/2/abs(a);