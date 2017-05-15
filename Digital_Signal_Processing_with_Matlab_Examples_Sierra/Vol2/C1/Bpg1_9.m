% Zeros of non symmetric FIR filters
% 4 cases
alfa=0:0.01:(2*pi);

subplot(2,2,1)
plot(cos(alfa),sin(alfa),'b'); hold on;
F=[1 0.8]; %FIR filter
[zi]=roots(F); 
plot(real(zi),imag(zi),'ko'); grid;
axis([-1.6 1.6 -1.6 1.6]);
title('zeros of non-symmetric FIR filters');

subplot(2,2,2)
plot(cos(alfa),sin(alfa),'b'); hold on;
F=[1 2.2 1.1]; %FIR filter
[zi]=roots(F) 
plot(real(zi),imag(zi),'ko');; grid;
axis([-1.6 1.6 -1.6 1.6]);

subplot(2,2,3)
plot(cos(alfa),sin(alfa),'b'); hold on;
F=[1 0.5 2]; %FIR filter
[zi]=roots(F); 
plot(real(zi),imag(zi),'ko');; grid;
axis([-1.6 1.6 -1.6 1.6]);

subplot(2,2,4)
plot(cos(alfa),sin(alfa),'b'); hold on;
F=[1 1 2.5 2 1]; %FIR filter
[zi]=roots(F); 
plot(real(zi),imag(zi),'ko');; grid;
axis([-1.6 1.6 -1.6 1.6]);


