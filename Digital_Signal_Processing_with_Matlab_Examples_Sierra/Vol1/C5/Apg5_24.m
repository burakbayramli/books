% train of impulses
fs=130; %sampling frequency in Hz.
fu=10; %signal frequency in Hz.
Ns=fs/fu; %number of samples per signal period
tiv=1/fs; %time intervals between samples

nsm=ceil(Ns/2);
u1=zeros(1,Ns); u1(nsm)=1; %impulse in the middle of zeros
u=[u1,u1,u1,u1,u1]; %signal with 5 periods
t=0:tiv:((5/fu)-tiv); %time intervals set (5 periods)
stem(t,u,'kx'); %plot impulse train
title('train of impulses'); xlabel('seconds');
axis([0 0.5 0 1.2]);
