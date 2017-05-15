%Propagation through nonlinearity
% and linearized propagation
% Asymmetrical result

%gaussian random data
Nd=80000;
rdat=randn(1,Nd);
bx=-1.5:0.05:1.5; %location of histogram bins
sig=0.5;
hsf=0.8;
adat=hsf+sig*randn(1,Nd); %with horizontal shift

%propagated random data
pdat=atan(adat); 
%histogram of posterior data
hpt=hist(pdat,bx); 
mup=mean(pdat); %mean of posterior data
[mx,ix]=max(hpt); %posterior histogram peak
ppx=bx(ix); ppy=mx; %"   " coordinates

%data through tangent
ttg=1/(1+hsf^2); tb=atan(hsf)-(ttg*hsf); %the tangent
tdat=ttg*(adat)+tb;
%histogram of data through tangent
htg=hist(tdat,bx); 
[mx,ix]=max(htg); %tdat histogram peak
ptx=bx(ix); pty=mx; %"   " coordinates

%------------------------------------------------------
figure(1)

plot(bx,hpt,'b'); hold on;
plot(bx,htg,'k');
plot(mup,0,'rx','Markersize',10); %posterior data mean
plot([ppx ppx],[0 ppy],'b--'); %line to post. hist. peak
plot([ptx ptx],[0 pty],'k--'); %line to tdat. hist. peak
title('nonlinear and linearized measurement'); 