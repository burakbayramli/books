t1=0:0.001:1;
t2=1:0.001:2;
t3=-1:0.001:0;
t4=2:0.001:3;
y1=(t1.^3)/6;
y2=(-5*t2.^3 +12*t2.^2 - 6*t2)/6;
y3=0*t3;
y4=0*t4;
plot(t1,y1,t2,y2,t3,y3,t4,y4);
xlabel('time in seconds'), 
ylabel('y(t)=x1(t) * x2(t) for all time');
