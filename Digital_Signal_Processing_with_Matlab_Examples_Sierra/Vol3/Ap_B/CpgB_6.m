%Propagation of sigma points through nonlinearity
%
%nonlinear function
vx=-10:0.1:10;
vy=atan(vx);

%gaussian random data
Nd=80000;
bx=-1.5:0.05:1.5; %location of histogram bins
sig=0.5;
adat=sig*randn(1,Nd);
%histogram of a priori data
Nbins=100;
[ha,hax]=hist(adat,Nbins); 

%sigma points:
xs0=0; 
xs1=xs0+sig;
xs2=xs0-sig;

%propagated random data
pdat=atan(adat);
%histogram of posterior data
hpt=hist(pdat,bx); 

%measured sigma points
ys0=atan(xs0);
ys1=atan(xs1);
ys2=atan(xs2);

figure(1)
pl1=0.1; pb1=0.35; pw1=0.2; ph1=0.55;
pl2=0.4; pb2=0.05; pw2=0.55; ph2=0.2;

subplot('position',[pl1 pb1 pw1 ph1]) %left plot
plot(hpt,bx,'k'); hold on;
%sigma projections
plot(0,0,'rx','MarkerSize',10);
plot(0,ys1,'rx','MarkerSize',10);
plot(0,ys2,'rx','MarkerSize',10);
plot([0 3500],[0 0],'r--');
plot([0 3500],[ys1 ys1],'b--');
plot([0 3500],[ys2 ys2],'b--');
axis([0 3500 -1.5 1.5]);
ylabel('after');

subplot('position',[pl2 pb1 pw2 ph1]) %central plot
plot(vx,vy,'k'); hold on;
%sigma projections
plot([-10 0],[0 0],'r--',-9.5,0,'r<');
plot([0 0],[-1.5 0],'r--',0,-0.4,'r^');
plot([xs1 xs1],[-1.5 ys1],'b--');
plot([xs2 xs2],[-1.5 ys2],'b--');
plot([-10 xs1],[ys1 ys1],'b--',-9.5,ys1,'b<');
plot([-10 xs2],[ys2 ys2],'b--',-9.5,ys2,'b<');

title('Propagation of sigma points through nonlinear measurement');

subplot('position',[pl2 pb2 pw2 ph2]) %bottom plot
plot(hax,ha,'k'); hold on;
%sigma points
plot(0,0,'rx','MarkerSize',10);
plot(xs1,0,'rx','MarkerSize',10);
plot(xs2,0,'rx','MarkerSize',10);
plot([0 0],[0 3500],'r--');
plot([xs1 xs1],[0 3500],'b--');
plot([xs2 xs2],[0 3500],'b--'); 
axis([-10 10 0 3500]);
title('before');
