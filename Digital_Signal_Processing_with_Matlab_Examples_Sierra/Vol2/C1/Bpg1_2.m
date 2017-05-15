% DFT matrix example
N=64; 
I=eye(N); %NxN identity matrix
D=fft(I); %the DFT matrix
R=real(D); %real part, for display purposes
imagesc(R); axis xy;  %matrix display as image
title('the 64x64 DFT matrix');

% test with a square signal
t=0:0.1:6.3;
y=square(t)';
Y1=fft(y); %the MATLAB fft
Y2=D*y; %the fft via matrix
dif=Y1-Y2; 
max_diference=max(dif)