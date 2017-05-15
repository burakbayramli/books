% Frequency response of band-stop Butterworth filter
wl=10; % desired low cut-off frequency
wh=100; %desired high cut-off frequency
wb=[wl wh]; %the stop band
N=10; % order of the filter (5+5)
[num,den]=butter(N,wb,'stop','s'); %analog band-stop Butterworth filter
w=logspace(0,3); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
axis([1 1000 0 1.1]);
grid;
ylabel('Gain'); xlabel('rad/s'); title('frequency response of 5th band-stop Butterworth filter');
