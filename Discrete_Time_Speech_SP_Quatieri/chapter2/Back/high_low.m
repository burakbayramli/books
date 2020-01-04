
sampling_rate = 10000;
lfft = 512; lfft2 = 256;
f = [ 0 .50 .60 1];
m = [1 1 0 0 ];
blow = remez(102, f, m);
blow = blow/sum(blow);
[a,b] = max(blow)
delta = zeros(1,length(blow));
delta(b) = 1.0;
bhigh = delta - blow;

subplot(411)
plot(((1:length(blow))/sampling_rate), blow) 
xlabel ('Time (sec)')
fblow = fft(blow, lfft);
subplot(412)
plot(((1:lfft2)/lfft)*10000, abs(fblow(1:lfft2))); 
xlabel ('Frequency (Hz)')

subplot(413)
plot(((1:length(bhigh))/sampling_rate), bhigh) 
xlabel ('Time (sec)')
fbhigh = fft(bhigh, lfft);
subplot(414)
plot(((1:lfft2)/lfft)*10000, abs(fbhigh(1:lfft2))); 
xlabel ('Frequency (Hz)')
