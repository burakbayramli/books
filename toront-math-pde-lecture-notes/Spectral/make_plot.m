
eps = 1/40;
dx = .0005;
x = 0:dx:1-dx;
y = 1./(eps^2+sin(pi*(x-1/2)).^2);
[k,amp] = find_spec(y);
figure(1)
plot(x,y,'LineWidth',2)
print -dps blow_up.ps
figure(2)
plot(k,amp,'LineWidth',2)
print -dps blow_up_spec.ps

I(1) = 0;
for j=2:length(x)
    I(j) = y(j)+I(j-1);
end
I = I*(x(2)-x(1));
figure(3)
clf
plot(x,I,'LineWidth',2)
print -dps sharp_transition.ps

v = ftrunc(y,50);
figure(4)
clf
plot(x,y,'LineWidth',2)
hold on
plot(x,v,'--','LineWidth',2)
axis([0,.5,-5,20])
print -dps underresolved_flat.ps

figure(5)
clf
plot(x,y,'LineWidth',2)
hold on
plot(x,v,'--','LineWidth',2)
max(abs((y-v)./y))
axis([.495,.505,1300,1605])
print -dps underresolved_peak.ps

% V(1) = 0;
% for j=2:length(x)
%     V(j) = v(j)+V(j-1);
% end
% V = V*(x(2)-x(1));
% figure(3)
% hold on
% plot(x,V,'r','LineWidth',2)

% v = ftrunc(y,100);
% figure(5)
% clf
% plot(x,(y-v)./y)
% max(abs((y-v)./y))
% 
% v = ftrunc(y,200);
% figure(6)
% clf
% plot(x,(y-v)./y)
% max(abs((y-v)./y))
% 
% v = ftrunc(y,300);
% figure(7)
% clf
% plot(x,(y-v)./y)
% max(abs((y-v)./y))




