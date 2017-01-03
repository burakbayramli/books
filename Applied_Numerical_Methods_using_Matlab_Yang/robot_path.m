%robot_path
clear
x1=[0  1  2]; y1=[0  1  4];  t1=[0  1  2]; ti1=[0: 0.05: 2];  
xi1=cspline(t1,x1,ti1);  yi1=cspline(t1,y1,ti1);
....................................
plot(xi1,yi1,'k', xi2,yi2,'b', xi3,yi3, 'k'), hold on
plot([x1(1) x2(1) x3(1) x3(end)],[y1(1) y2(1) y3(1) y3(end)],'o')
plot([x1 x2 x3],[y1 y2 y3],'k+'), axis([0 5 0 5])
