function xp=lorenz(t,x)
xp(1)=-10*x(1)+10*x(2);
xp(2)=-x(1)*x(3) + 28*x(1)-x(2);
xp(3)=x(1)*x(2)-8/3*x(3);

