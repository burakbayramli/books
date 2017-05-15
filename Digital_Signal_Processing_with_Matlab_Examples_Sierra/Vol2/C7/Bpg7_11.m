% Scatterplot of original sources and
% scatterplot of mixed signals
% example of two speeches

%read two sound files
[a,fs1]=wavread('spch1.wav'); %read wav file
[b,fs1]=wavread('spch2.wav'); % "  "  "
R=2; %reduce data size for clearer picture
a=decimate(a,R);
b=decimate(b,R);
s1=(a-mean(a))'; %zero-mean
s2=(b-mean(b))'; % " "  "
vr1=var(s1); s1=s1/sqrt(vr1); %variance=1
vr2=var(s2); s2=s2/sqrt(vr2); %"  "  "

s=[s1;s2]; %combine sources

%mix of sources
N=length(s1); 
M=[0.7 0.3; 0.2 0.8]; %example of mixing matrix
x=M*s; %mixed signals

%display
figure(1)
subplot(1,2,1); 
%scatterplot of sources
plot(s(1,:),s(2,:),'k.'); hold on; %scatterplot
L=3;
%axes
plot([-L L],[0 0],'k');
plot([0 0],[-L L],'k');
axis([-L  L -L L]);
title('scatterplot of 2 sources'); 
xlabel('s1'); ylabel('s2');

subplot(1,2,2); 
%scatterplot of mixed signals
plot(x(1,:),x(2,:),'k.'); hold on; %scatterplot
L=3;
%axes
plot([-L L],[0 0],'k');
plot([0 0],[-L L],'k');
axis([-L  L -L L]);
title('scatterplot of mixed signals'); 
xlabel('s1'); ylabel('s2');


