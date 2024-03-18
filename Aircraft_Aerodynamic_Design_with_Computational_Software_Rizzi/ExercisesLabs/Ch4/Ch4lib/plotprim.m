function plotprim(handles)
W      = handles.Data.W;
a = handles.Data.a;
p0 = handles.Data.p;
gamma = 1.4;
r  = W(:,1);
u  = W(:,2)./r;
re = W(:,3);
p  = (gamma-1)*(re-r.*u.^2/2);
subplot(311)
plot(r,'+-k')
hold on
subplot(312)
plot(u,'+-k')
hold on
subplot(313)
plot(p,'+-k')
hold on
plot(p0,'+-r')
