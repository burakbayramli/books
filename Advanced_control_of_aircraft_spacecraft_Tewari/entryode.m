function dydx=entryode(x,y)
  global R;
  global mu;
  global r0;
  dydx=[y(2)*sin(y(3))
	-0.5*y(5)/R-mu*sin(y(3))/y(1)^2
	(y(2)-mu/(y(1)*y(2)))*cos(y(3))/y(1)
	-2*mu*y(5)*sin(y(3))/y(1)^3
	-y(4)*sin(y(3))];

function res=entrybc(ya,yb)
  global r0;
  dtr=pi/180;
  res=[ya(1)-r0-100
       ya(2)-8
       ya(3)+0.1*dtr
       yb(1)-r0-5
       yb(2)-2];
