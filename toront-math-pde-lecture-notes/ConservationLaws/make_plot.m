x = -5:.1:5;
figure(1)
clf
plot(x,2*x,'LineWidth',2)
hold on

X = -5:.1:1;
plot(X,1+X,':')
X = -5:.1:2;
plot(X,2+X,':')
X = -5:.1:5;
plot(X,3+X,':')

X = -5:.1:2;
plot(ones(size(X)),X,':')
X = -5:.1:4;
plot(2*ones(size(X)),X,':')
X = -5:.1:6;
plot(3*ones(size(X)),X,':')
axis([-5,5,0,5])

text(-2,2,'u(x,t)=1','FontSize',16)
text(2,2,'u(x,t)=0','FontSize',16)

xlabel('x','FontSize',16)
ylabel('t','FontSize',16)

figure(1)

print -dps Rankine_Hugoniot.ps
