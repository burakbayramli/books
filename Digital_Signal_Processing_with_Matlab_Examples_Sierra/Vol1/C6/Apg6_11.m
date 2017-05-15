%Hear and plot vowel signal
[y1,fs1]=wavread('i.wav'); %read wav file
soundsc(y1,fs1); %hear wav

Ny=length(y1);
tiv=1/fs1;
t=0:tiv:((Ny-1)*tiv); %time intervals set

figure(1)
subplot(2,1,1)
plot(t,y1,'k'); %plots the signal
axis([0 (Ny*tiv) -0.8 0.8]);
title('vowel sound');
ylabel('signal'); xlabel('seconds')

subplot(2,1,2)
ta=5000; tb=5800;
plot(t(ta:tb),y1(ta:tb),'k'); %plots a zoom on the signal
axis([(ta*tiv) (tb*tiv) -0.8 0.8]);
ylabel('signal'); xlabel('seconds')

