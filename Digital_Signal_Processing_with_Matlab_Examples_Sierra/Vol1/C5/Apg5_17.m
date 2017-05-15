% Comparison of Hw(w) of Kaiser window
fs=130; %sampling frequency in Hz.
N=50; %even order
f=logspace(0,2,400); %logaritmic set of frequency values in Hz.
dend=[1]; %transfer function denominator

for beta=1:6,
hw=kaiser(N+1,beta); %Kaiser window
numd=2*hw/N; %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
subplot(2,3,beta);semilogx(f,abs(G),'k'); %plots gain
axis([1 100 0 0.5]);
grid;
end

title('stop-band of Hw(w) of 50th Kaiser window');
xlabel('Hz.');
