% frequency response of raised cosine filter
fs=130; %sampling frequency in Hz.
fc=10; %cut-off at 10 Hz.
N=50; %even order
beta=0; %roll-off factor
numd=firrcos(N,fc,beta,fs,'rolloff'); %transfer function numerator
dend=[1]; %transfer function denominator

f=logspace(0,2,200); %logaritmic set of frequency values in Hz.
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'k'); hold on; %plots gain
axis([1 100 0 1.2]); grid;
ylabel('Gain'); xlabel('Hz.'); title('Hf(w) 50th raised-cosine filter')

for beta=0.25:0.25:1,
numd=firrcos(N,fc,beta,fs,'rolloff'); %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'k');  %plots gain
end

