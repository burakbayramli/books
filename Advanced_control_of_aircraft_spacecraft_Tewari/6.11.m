% Program for simulating perturbed trajectory with a constant gain,
% planar orbit regulator for an elliptical nominal orbit,
% using Runge-Kutta 4(5) scheme, 'ode45.m'.
% (c) 2009 Ashish Tewari
global f9;f9 = fopen('u.txt', 'w');
OPTIONS = odeset('RelTol', 1e-8);
[t,x]=ode45(@orbitplantfixk,[0 5829],[8500,9,80000]','OPTIONS');
n=size(t,1);
rn=[];rndot=[];hn=[];
for i=1:n
[rn(i,1), rndot(i,1), hn(i,1)]=nominalorbit(t(i,1));
end
N=round(n/6);
subplot(311),plot(t(1:N,1),x(1:N,1),t(1:N,1),rn(1:N,1),':'),
ylabel('r (km)'),hold on,
subplot(312),plot(t(1:N,1),x(1:N,2),t(1:N,1),rndot(1:N,1),':'),
ylabel('dr/dt (km/s)'),hold on,
subplot(313),plot(t(1:N,1),x(1:N,3),t(1:N,1),hn(1:N,1),':'),
xlabel('Time (s)'),ylabel('h (km^2/s)')
r=x(:,1);rdot=x(:,2);h=x(:,3);
v=sqrt(rdot.^2+(h./r).^2);
vn=sqrt(rndot.^2+(hn./rn).^2);
tanphi=rdot.*r./h;
tanphin=rndot.*rn./hn;
phi=atan(tanphi);
phin=atan(tanphin);
figure
plot(v.*cos(phi),v.*sin(phi),vn.*cos(phin),vn.*sin(phin),':'),
xlabel('v cos\phi'),ylabel('v sin\phi'),;
fclose('all');
load u.txt -ascii
figure
n=round(size(u,1)/6);
plot(u(1:10:n,1),1000*u(1:10:n,2)),xlabel('Time (s)'),
ylabel('f_\mathrmT/m (m/s^2)')
figure
plot(u(1:10:n,1),u(1:10:n,3)),xlabel('Time (s)'),
ylabel('\alpha (deg.)')
