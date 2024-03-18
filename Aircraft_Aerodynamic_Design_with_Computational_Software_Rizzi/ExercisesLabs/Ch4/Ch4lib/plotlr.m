function plotlr(handles)
ls = handles.Data.ls;
rs = handles.Data.rs;
p  = handles.Data.p;
gamma = 1.4;
rl = ls(:,1);
ul = ls(:,2);
pl = ls(:,3);
rr = rs(:,1);
ur = rs(:,2);
pr = rs(:,3);
subplot(311)
plot(rl,'o-b')
plot(rr,'.-r')
subplot(312)
plot(ul,'o-b')
plot(ur,'.-r')
subplot(313)
plot(pl,'o-b')
plot(pr,'.-r')
