%
%  Program name - prob8261a.m  (modified problem 8.26(1), p553)
%  Comparison of characteristic features of discrete-time Butterworth, 
%  Chebyshev 1,Chebyshev 2 and elliptic lowpass filters. 
%
Fs=8000; 
fp=500/4000; fs=2000/5000;
Ap=0.1; As=60;
%
% Calculate filter coefficients
[N1, wc1]=buttord(fp, fs, Ap, As);	% filter order
[N2, wc2]=cheb1ord(fp, fs, Ap, As);
[N3, wc3]=cheb2ord(fp, fs, Ap, As);
[N4, wc4]=ellipord(fp, fs, Ap, As);
[b1, a1]=butter(4, fp);					% Butterworth filter
[b2, a2]=cheby1(4, Ap, fp);
[b3, a3]=cheby2(4, As, fs);
[b4, a4]=ellip(4, Ap, As, fp);
%
% Calculate frequency responses
%
[H1, f1]=freqz(b1, a1, 512, Fs);
[H2, f2]=freqz(b2, a2, 512, Fs);
[H3, f3]=freqz(b3, a3, 512, Fs);
[H4, f4]=freqz(b4, a4, 512, Fs);
%
% Plot magnitude frequency responses
%
figure (1);
plot(f1, 20*log10(abs(H1)), 'r:')	% Plot Butterworth magnitude response
hold on
figure (1);
plot(f2, 20*log10(abs(H2)), 'b--')	% Plot Cheby1 magnitude response
hold on
figure (1);
plot(f3, 20*log10(abs(H3)), 'g-.')	% Plot Cheby2 magnitude response
hold on
figure (1);
plot(f4, 20*log10(abs(H4)), 'k-')	% Plot elliptic magnitude response
hold on
legend('Butworth', 'Cheby1', 'Cheby2', 'Ellip');
axis([0 4000 -100 10])
ylabel('Magnitude (dB)')
xlabel('Frequency (Hz)')
title('Filter magnitude responses')
hold off;
%
% Plot phase responses
%
figure (2);
plot(f1, angle(H1)*180/pi, 'r:')		% Plot BZT phase response
hold on
figure (2);
plot(f2, angle(H2)*180/pi, 'b--')	% Plot Cheby 1 phase response
hold on
figure (2);
plot(f3, angle(H3)*180/pi, 'g-.')	% Plot Cheby 2 phase response
hold on
figure (2);
plot(f4, angle(H4)*180/pi, 'k-')		% Plot elliptic phase response
hold on
figure(2);
legend('Butter', 'Cheby1','Cheby2', 'Ellip');
axis([0 4000 -360 360])
ylabel('Phase (Degrees)')
xlabel('Frequency (Hz)')
title('Filter Phase Responses')
hold off
% 
% Plot pole-zero diagrams
%
figure (3);
zplane(b1, a1)
title('Pole-zero diagram - Butterworth filter')
figure (4);
zplane(b2, a2)
title('Pole-zero diagram - Chebyshev 1 filter')
figure (5);
zplane(b3, a3)
title('Pole-zero diagram - Chebyshev 2 filter')
figure (6);
zplane(b4, a4)
title('Pole-zero diagram - Elliptic filter')
N1
N2
N3
N4

