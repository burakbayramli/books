clear

dx = .01;
x = 0:dx:1-dx;
y = exp(cos(2*pi*x));
[k,amp] = find_spec(y);
figure(1)
plot(x,y,'LineWidth',2)
figure(2)
plot(k,amp,'o-')
axis([0,30,-17,1])

pause

y = 1./(2-cos(2*pi*x));
[k,amp] = find_spec(y);
figure(1)
plot(x,y,'LineWidth',2)
figure(2)
plot(k,amp,'o-')
axis([0,40,-17,1])

pause

x = 0:dx:1-dx;
eps = 1;
y = 1./(eps^2+sin(pi*(x-1/2)).^2);
[k,amp] = find_spec(y);
figure(1)
plot(x,y,'LineWidth',2)
figure(2)
plot(k,amp,'o-')
% k_crit = 20

eps = 1/2;
x = 0:dx:1-dx;
y = 1./(eps^2+sin(pi*(x-1/2)).^2);
[k,amp] = find_spec(y);
figure(3)
plot(x,y,'LineWidth',2)
figure(4)
plot(k,amp,'o-')
% k_crit = 40

eps = 1/4;
dx = dx/2;
x = 0:dx:1-dx;
y = 1./(eps^2+sin(pi*(x-1/2)).^2);
[k,amp] = find_spec(y);
figure(5)
plot(x,y,'LineWidth',2)
figure(6)
plot(k,amp,'o-')
% k_crit = 80

eps = 1/8;
dx = dx/2;
x = 0:dx:1-dx;
y = 1./(eps^2+sin(pi*(x-1/2)).^2);
[k,amp] = find_spec(y);
figure(5)
plot(x,y,'LineWidth',2)
figure(6)
plot(k,amp,'o-')
% k_crit = 160








M = 20;
y = zeros(size(x));
x1 = .3179;
x2 = .8751;
eps = .02;
for m=-M:M
    y = y + tanh((x-x1-m)/eps) - tanh((x-x2-m)/eps);
end
y = y-1;
figure(1)
plot(x,y,'LineWidth',2)
[k,amp] = find_spec(y);
figure(2)
plot(k,amp,'-')

pause

dx = dx/5;
x = 0:dx:1-dx;
M = 20;
y = zeros(size(x));
x1 = .3179;
x2 = .8751;
eps = .02;
for m=-M:M
    y = y + tanh((x-x1-m)/eps) - tanh((x-x2-m)/eps);
end
y = y-1;
figure(1)
plot(x,y,'LineWidth',2)
[k,amp] = find_spec(y);
figure(2)
plot(k,amp,'-')

pause

figure(1)
clf
x = 0:.1:8;
k = -0;
plot(x,cos((k+8)*2*pi*x/8),'--','LineWidth',2);
hold on
plot(x,cos(-k*2*pi*x/8),'LineWidth',2);
X = 0:1:8;
plot(X,cos(-k*2*pi*X/8),'o','LineWidth',2);
title('cos(0 x) and cos(8 x)','FontSize',16)

figure(2)
clf
x = 0:.1:8;
k = -1;
plot(x,cos((k+8)*2*pi*x/8),'--','LineWidth',2);
hold on
plot(x,cos(-k*2*pi*x/8),'LineWidth',2);
X = 0:1:8;
plot(X,cos(-k*2*pi*X/8),'o','LineWidth',2);
title('cos(1 x) and cos(7 x)','FontSize',16)

figure(3)
clf
x = 0:.1:8;
k = -2;
plot(x,cos((k+8)*2*pi*x/8),'--','LineWidth',2);
hold on
plot(x,cos(-k*2*pi*x/8),'LineWidth',2);
X = 0:1:8;
plot(X,cos(-k*2*pi*X/8),'o','LineWidth',2);
title('cos(2 x) and cos(6 x)','FontSize',16)

figure(4)
clf
x = 0:.1:8;
k = -3;
plot(x,cos((k+8)*2*pi*x/8),'--','LineWidth',2);
hold on
plot(x,cos(-k*2*pi*x/8),'LineWidth',2);
X = 0:1:8;
plot(X,cos(-k*2*pi*X/8),'o','LineWidth',2);
title('cos(3 x) and cos(5 x)','FontSize',16)

