%Hear & see transformer signal
[y1,fs1]=wavread('transformer1.wav'); %read wav file
soundsc(y1,fs1); %hear wav

Ny=length(y1);
tiv=1/fs1;
t=0:tiv:((Ny-1)*tiv); %time intervals set

subplot(2,1,1)
plot(t,y1,'k'); %plots the signal
axis([0 (Ny*tiv) -0.4 0.4]);
title('transformer sound');
ylabel('signal'); xlabel('seconds')

subplot(2,1,2)
nyz=ceil(Ny/40); %zoom
plot(t(1:nyz),y1(1:nyz),'k'); %plots a zoom on the signal
axis([0 (nyz*tiv) -0.25 0.25]);
ylabel('signal'); xlabel('seconds')




