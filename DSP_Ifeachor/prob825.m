%
%  Program name - prob825.m  (problem 8.25(1), p553)
%  Comparison of magnitude and phase responses of an analogue 
%  and equivalent BZT and impulse invariant discrete-time filters 
%  and pole-zero diagrams (elliptic low pass filter) 
%
Fs=10000; FN=Fs/2;
fp=1000; fs=3000;
wp=fp*2*pi; ws=fs*2*pi;
Ap=1; As=60;
%
% Calculate filter coefficients and frequency responses
[N, wc]=ellipord(wp, ws, Ap, As,'s');	% analog filter
[B, A]=ellip(N, Ap, As, wc, 's');
[bBZT, aBZT]=bilinear(B,A,Fs);			% BZT filter
[bIIT,aIIT]=impinvar(B,A,Fs);				% Impulse invariance filter
%
% Compute frequency response
[Ha, wa]=freqs(B,A);	
[HBZT, fBZT]=freqz(bBZT, aBZT, 512, Fs);
[HIIT, fIIT]=freqz(bIIT, aIIT, 512, Fs);	
%
% Plot magnitude frequency responses
%
figure(1);										% Plot analogue magnitude response
plot(wa/(2*pi), 20*log10(abs(Ha)))
hold on
figure (1);
plot(fBZT, 20*log10(abs(HBZT)), 'r:')	% Plot BZT magnitude response
hold on
figure (1);
plot(fIIT, 20*log10(abs(HIIT)), 'g:')		% Plot Impinv magnitude response
legend('Analog', 'BZT', 'Imp Invar');
axis([0 10000 -120 0])
ylabel('Magnitude (dB)')
xlabel('Frequency (Hz)')
title('Filter magnitude responses')
hold off;
%
% Plot phase responses
%
figure(2);
plot(wa/(2*pi), angle(Ha)*180/pi)		% Plot analogue phase response
hold on
figure (2);
plot(fBZT, angle(HBZT)*180/pi, 'r:')		% Plot BZT phase response
hold on
figure(2);
plot(fIIT, angle(HIIT)*180/pi, 'g.')			%ImpInvar phase response
legend('Analog', 'BZT','Imp Invar');
axis([0 10000 -360 360])
ylabel('Phase (Degrees)')
xlabel('Frequency (Hz)')
title('Filter Phase Responses')
hold off
% 
% Plot pole-zero diagrams
%
figure (3);
zplane(bBZT, aBZT)
title('Pole-zero diagram - BZT filter')
figure (4);
zplane(bIIT, aIIT)
xmin=-1; xmax=1; ymin=-1; ymax=1; % Scale the z-plane for the Impulse Inva.
axis([xmin xmax ymin ymax])
title('Pole-zero diagram - impulse invariance filter')
