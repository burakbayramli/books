% Comparison of Hf(w) of Kaiser window
fs=130; %sampling frequency in Hz.
fc=10/(fs/2); %cut-off at 10 Hz.
N=50; %even order
f=logspace(0,2); %logaritmic set of frequency values in Hz.
dend=[1]; %transfer function denominator

for beta=1:6,
hw=kaiser(N+1,beta); %Kaiser window
numd=fir1(N,fc,hw); %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
subplot(2,3,beta);semilogx(f,abs(G),'k'); %plots gain
axis([1 100 0 1.2]);
grid;
end

title('Hf(w) of 50th Kaiser windowed filter');
xlabel('Hz.');
