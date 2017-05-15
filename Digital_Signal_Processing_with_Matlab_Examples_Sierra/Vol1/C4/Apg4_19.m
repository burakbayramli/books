% Frequency response of high-pass Butterworth filter
wh=100; %desired high cut-off frequency
N=5; % order of the filter 
[num,den]=butter(N,wh,'high','s'); %analog high-pass Butterworth filter
w=logspace(1,3); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
axis([10 1000 0 1.1]);
grid;
ylabel('Gain'); xlabel('rad/s'); title('frequency response of 5th high-pass Butterworth filter');
