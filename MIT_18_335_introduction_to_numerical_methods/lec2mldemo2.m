% MIT 18.335 - Lecture 2 MATLAB Demo 2
% Demonstrate induced matrix norms
% Per-Olof Persson, September 10, 2007

n=128;
phi=2*pi*(0:n)'/n;

% 2-norm

x=cos(phi); y=sin(phi);
plot(x,y),axis equal
pause

A=[1.5,.75; -.4,-1.1];

x1=A(1,1)*x+A(1,2)*y;
y1=A(2,1)*x+A(2,2)*y;

plot(x,y,x1,y1),axis equal
pause

[U,S,V]=svd(A);
line([0,V(1,1)],[0,V(2,1)],'color','b');
pause
line(S(1,1)*[0,U(1,1)],S(1,1)*[0,U(2,1)],'color',[0,.5,0]);
pause

S(1,1)
norm(A,2)
pause

% 1-norm

x=cos(phi); y=sin(phi);
l=abs(x)+abs(y);
x=x./l; y=y./l;

plot(x,y),axis equal
pause

x1=A(1,1)*x+A(1,2)*y;
y1=A(2,1)*x+A(2,2)*y;

plot(x,y,x1,y1),axis equal
pause

[maxval,ix]=max(abs(x1)+abs(y1));

[x(ix),y(ix)]
[x1(ix),y1(ix)]
pause

line([0,x(ix)],[0,y(ix)],'color','b');
pause
line([0,x1(ix)],[0,y1(ix)],'color',[0,.5,0]);
pause

maxval
norm(A,1)
pause

% inf-norm

x=cos(phi); y=sin(phi);
l=max(abs(x),abs(y));
x=x./l; y=y./l;

plot(x,y),axis equal
pause

x1=A(1,1)*x+A(1,2)*y;
y1=A(2,1)*x+A(2,2)*y;

plot(x,y,x1,y1),axis equal
pause

[maxval,ix]=max(max(abs(x1),abs(y1)));

[x(ix),y(ix)]
[x1(ix),y1(ix)]
pause

line([0,x(ix)],[0,y(ix)],'color','b');
pause
line([0,x1(ix)],[0,y1(ix)],'color',[0,.5,0]);
pause

maxval
norm(A,inf)
pause
