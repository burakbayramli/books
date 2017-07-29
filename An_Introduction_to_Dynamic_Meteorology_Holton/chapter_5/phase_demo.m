% MATLAB file:  phase_demo.m
% Simple program to illustrate phase for function of the form
% f(x) =  Asin(kx) + Bcos(kx) = real(C exp(ikx)),
% where A and B are real constants and C is complex.
clear all
close all
kx = linspace(-pi,pi,30);

phase =  input('enter a phase angle in degrees  ')*pi/180;

disp(['phase in radians = ',num2str(phase)])

disp('real and imaginary parts of C')

C = cos(phase)+i*sin(phase)

A = -imag(C); 
B = real(C);
figure(1)

f1 = real(C.*exp(i*kx));        % complex exponential form of f(x)

f2 = A*sin(kx) + B*cos(kx);     % traditional form of f(x)

%f3  to be calculated by student

subplot(3,1,1)
plot(kx*180/pi,f1), xlabel('phase in degrees'), ylabel('amplitude')
title('f(x) = real(C exp(ikx)) ')
subplot(3,1,2)
plot(kx,f2), xlabel('phase in radians'), ylabel('amplitude')
title('  f(x) = Asin(kx)+Bcos(kx) ')
subplot(3,1,3)
%f3 to be plotted here



