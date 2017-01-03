function b=fir_weights_2(H,windo)
%b=fir_weights_2(H,windo)
%
%Generates linear-phase FIR weights corresponding with a given
%amplitude gain vector.
%
% b    =Linear-phase FIR filter windowed weights as a row vector.
% H    =Amplitude gain vector. The elements of H must be evenly spaced
%       at frequencies ranging from v=0 to v=0.5 Hz-s.
%       H must be sampled often enough to produce a smooth plot
%         of the amplitude gain function.
%       H (ampl. gain) must be real.  If it isn't, its magnitude is used.
% windo=1 (boxcar),  2 (tapered),  3(tent)  4 (hanning),
%       5 (hamming), 6(blackman), or 7(kaiser) 
%         (In the last case windo is a vector=[7,beta], with beta
%          in range [4.0,9.0]. See Eq. 5.8 in the text.)

%parameters
thr=.01;                                %negligible-weight threshold

%b1= symmetric inverse transform of amplitude gain
H=abs(H(:));
L=length(H);
H2=[H;H(L:-1:2)];                       %length(H2)=2L-1
b0=ifft(H2);
b1=fftshift(b0);                        %length(b1)=2L-1

%b2=truncated version of b1
b2min=thr*max(abs(b1));
n=1;
while abs(b1(n))<b2min
    n=n+1;
end
b2=b1(n:2*L-n);                         %length(b2)=

%b=windowed version of b2
b=b2.*window(2*(L-n)+1,windo)';
