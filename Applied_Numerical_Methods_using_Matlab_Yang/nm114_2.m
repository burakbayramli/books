%nm114_2: plot a circle
clear
figure(1), clf
r=2; th=[0: .02:1]*pi;
plot(r*exp(j*th),'r-'), hold on
plot(r*cos(th+pi),r*sin(th+pi),'m.')
x=1; y=-1; 
[s,ermsg]=sprintf('(%1d,%1d)',x,y);
text(x,y,s)
plot([-2 2],[0 0],'b:')
plot([0 0],[-2 2],'k-.')
plot([-2 2],[-2 2],'c--')
plot(-1,1,'h'), plot(1,1,'p')
shg, pause, clf
subplot(221), polar(th,exp(-th))
subplot(222), semilogx(exp(th))
subplot(223), semilogy(exp(th))
subplot(224), loglog(exp(th))
pause, clf
subplot(221), stairs([1 3 2 0])
subplot(222), stem([1 3 2 0])
subplot(223), bar([2 3; 4 5])
subplot(224), barh([2 3; 4 5])
pause, clf
y=[0.3 0.9 1.6 2.7 3 2.4];
subplot(221), hist(y,3)
subplot(222), hist(y,0.5+[0 1 2])