%Propagation through nonlinearity
%
%nonlinear function
vx=-10:0.1:10;
vy=atan(vx);

%gaussian random data
Nd=40000;
bx=-1.5:0.05:1.5; %location of histogram bins
sig=0.5;
adat=sig*randn(1,Nd);
%histogram of a priori data
Nbins=50;
[ha,hax]=hist(adat,Nbins); 

%propagated random data
pdat=atan(adat);
%histogram of posterior data
hpt=hist(pdat,bx); 

figure(1)
pl1=0.1; pb1=0.35; pw1=0.2; ph1=0.55;
pl2=0.4; pb2=0.05; pw2=0.55; ph2=0.2;

subplot('position',[pl1 pb1 pw1 ph1]) %left plot
plot(hpt,bx); hold on;
plot([0 3500],[0 0],'r--');
axis([0 3500 -1.5 1.5]);
ylabel('after');

subplot('position',[pl2 pb1 pw2 ph1]) %central plot
plot(vx,vy); hold on;
plot([-10 0],[0 0],'r--',-9.5,0,'r<');
plot([0 0],[-1.5 0],'r--',0,-0.4,'r^');
title('nonlinear measurement');

subplot('position',[pl2 pb2 pw2 ph2]) %bottom plot
plot(hax,ha); hold on;
plot([0 0],[0 3500],'r--');
axis([-10 10 0 3500]);
title('before');
