function myarrow(x,y)
x=x(:); y=y(:);
hd = y;
line([x(1) hd(1)], [x(2) hd(2)],'color','k')
% get the normal to the vector
v = y-x; v=v./sqrt(v'*v);
n(1,1) = -v(2); n(2,1) = v(1);
h = 0.025;
p1 = hd + h*(0.5*n-v);
p2 = hd - h*(0.5*n+v);
line([hd(1),p1(1)], [hd(2),p1(2)],'color','k')
line([hd(1),p2(1)], [hd(2),p2(2)],'color','k')
line([p1(1) p2(1)], [p1(2),p2(2)],'color','k')