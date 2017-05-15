%Propagation through nonlinearity
% Asymmetrical result
%
%nonlinear function
vx=-10:0.1:10;
vy=atan(vx); 

%gaussian random data
Nd=40000;
bx=-1.5:0.05:1.5; %location of histogram bins
sig=2;
hsf=0.8;
adat=hsf+sig*randn(1,Nd); %with horizontal shift
%histogram of a priori data
Nbins=50;
[ha,hax]=hist(adat,Nbins); 

%propagated random data
pdat=atan(adat); 
%histogram of posterior data
hpt=hist(pdat,bx); 
mup=mean(pdat); %mean of posterior data

figure(1)
pl1=0.1; pb1=0.35; pw1=0.2; ph1=0.55;
pl2=0.4; pb2=0.05; pw2=0.55; ph2=0.2;

cnl=atan(hsf); %shifted arrow position

subplot('position',[pl1 pb1 pw1 ph1]) %left plot
plot(hpt,bx); hold on;
plot([0 3500],[cnl cnl],'r--');
plot(0,mup,'rx','Markersize',10);
axis([0 3500 -1.5 1.5]);
ylabel('after');

subplot('position',[pl2 pb1 pw2 ph1]) %central plot
plot(vx,vy); hold on;
plot([-10 hsf],[cnl cnl],'r--',-9.5,cnl,'r<');
plot([hsf hsf],[-1.5 cnl],'r--',hsf,-0.4,'r^');
title('nonlinear measurement');

subplot('position',[pl2 pb2 pw2 ph2]) %bottom plot
plot(hax,ha); hold on;
plot([hsf hsf],[0 3500],'r--');
axis([-10 10 0 3500]);
title('before');

