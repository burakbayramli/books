function bld050301
% Zwei Kegel, die sich schneiden
clf
set(gcf,'renderer','zbuffer')
Z = [0;0;1];
r = 1;
s = 3;
kegel1(Z,r,s)
Z = [0.4;-0.3;1];
s = 1.7;
r = 1.2;
kegel2(Z,r,s)
Z = [0;0;1];
r = 1;
s = 3;
kegel3(Z,r,s)
grid on
xlabel('x','fontsize',22)
ylabel('y','fontsize',22)
zlabel('z','fontsize',22)
axis equal tight
