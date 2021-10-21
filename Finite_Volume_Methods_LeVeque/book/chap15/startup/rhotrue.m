

pl = .4*(data(1,3) - .5*data(1,2)^2/data(1,1));
ul = data(1,2)/data(1,1);
cl = sqrt(1.4*pl/data(1,1));
ulmcl = ul-cl;
ss = (data(mx,2)-data(1,2)) / (data(mx,1)-data(1,1));

rhol = data(1,1);
rhor = data(mx,1);
hold on
plot([-100 ss ss 100], [rhol rhol rhor rhor])
hold off

