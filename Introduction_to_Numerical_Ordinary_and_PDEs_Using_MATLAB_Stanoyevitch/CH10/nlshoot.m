function xp=nlshoot(t,x)
xp(1)=x(2);
xp(2)=-2*(x(1)*x(2)+t*x(2)+x(1)+t);
xp(3)=x(4);
xp(4)=-2*(x(2)+1)*x(3)-2*(x(1)+t)*x(4);

