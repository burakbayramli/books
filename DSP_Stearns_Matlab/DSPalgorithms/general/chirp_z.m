function [X,v]=chirp_z(x,v1,v2,N)
%[X,v]=chirp_z(x,v1,v2,N)
%
%Computes the chirp-z transform of a vector x in terms of samples of the 
% contiuous DFT.
%
%Inputs:  x  =signal vector to be transformed
%         v1 =low end of spectrum; 0<=v1<.5 Hz-s.
%         v2 =upper end of spectrum; v1<v2<=.5
%         N  =# points in spectrum
%
%Outputs: X  =DFT at N equally spaced points from v1 thru v2
%         v  =N equally spaced frequencies in Hz-s.
%
%References: 
%Oppenheim & Schafer, Discrete-Time Signal Processing, Prentice-Hall, 1989,
%  p.623ff.
%Stearns, Digital Signal Analysis (2nd ed), Prentice-Hall, 1990, p.138ff.

%check for errors
if nargin<4,
    error('Four arguments are required.');
elseif length(x)~=numel(x),
    error('x must be a vector');
elseif v1<0 | v2<=v1 | v2>.5,
    error('0<=v1<v2<=.5 must be true for frequencies v1 and v2.');
end

%initialize
x=x(:);                                     %assure x is a column vector
K=length(x);
v=linspace(v1,v2,N)';
dv=(v2-v1)/(N-1);                           %increment on |z|=1 (Hz-s)

%comments below refer to Digital Signal Analysis (2nd ed), p.138ff
A=exp(j*2*pi*v1);                           %eq. (7.20)
B=exp(-j*2*pi*dv);                          %eq. (7.20)
L=2^nextpow2(K+N-1);                        %step 1 on p.141
n=(0:K-1)';
g1=A.^(-n);
g2=B.^(n.^2/2);
g=[g1.*g2.*x; zeros(L-K,1)];                %step 2
G=fft(g);                                   %step 3
b1=B.^(-((0:N-1)'.^2)/2);                   %step 4a
b2=zeros(L-K-N+1,1);                        %step 4b
b3=B.^(-((L-(L-K+1:L-1)').^2)/2);           %step 4c
b=[b1;b2;b3];                               %step 4 complete
B1=fft(b);                                  %step 5
H=G.*B1;                                    %step 6
h=ifft(H);                                  %step 7
X=h(1:N).*B.^((0:N-1)'.^2/2);               %step 8
