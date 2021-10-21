

dt = t;
ubar = 1;

qright  = 15*pi/4;
qleft = pi/4;
q0 = linspace(qleft, qright, 500);
x0 = [-3 0*q0 3];
q0 = [qleft q0 qright];
s = cos(q0);
x1 = x0 + s*dt;

%plot(x0,q0,'--');
hold on
%plot([-1 0], [qleft qleft])
%plot([0 1], [qright qright])

%plot(x1,q0)

q1 = fzero('fsinroot',3*pi/2);
f1 = sin(q1);
q2 = 3*pi/2;
f2 = -1;
q3 = 7*pi/2;
f3 = -1;

s1 = cos(q1);  % shock speed
s2 = 0;

qrare1 = linspace(q1,q2,100);
xrare1 = dt*cos(qrare1);
qrare2 = linspace(q3,qright,100);
xrare2 = dt*cos(qrare2);
qt1 = [qleft qleft q1 qrare1 q2 q3 qrare2 qright];
xt1 = [-3 s1*dt s1*dt xrare1 s2*dt s2*dt xrare2 3];
hline = plot(xt1,qt1);
%set(hline,'LineWidth',2);


hold off

axis([-1.5 1.5 0 14])
%print fsinclaw -deps

