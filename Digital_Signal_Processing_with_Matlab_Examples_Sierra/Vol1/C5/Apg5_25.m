% response of the raised cosine filter to the train of impulses
fs=130; %sampling frequency in Hz.
fu=10; %signal frequency in Hz.
Ns=fs/fu; %number of samples per signal period
tiv=1/fs; %time intervals between samples

% the train of impulses
nsm=ceil(Ns/2);
u1=zeros(1,Ns); u1(nsm)=1; %impulse in the middle of zeros
u=[u1,u1,u1,u1,u1]; %signal with 5 periods
t=0:tiv:((5/fu)-tiv); %time intervals set (5 periods)

% the filter
fc=fu; %cut-off frequency
beta=0.5; %roll-off factor
N=50; %even order
numd=firrcos(N,fc,beta,fs,'rolloff'); %transfer function numerator
dend=[1]; %transfer function denominator

% the filter output
y=filter(numd,dend,u);
plot(t,y,'-kx')
title('raised cosine filter response'); xlabel('seconds');

