
% GABORFILT  Gabor Filtering
%            Y = GABORFILT(X, F, A, S) filters the data in vector X
%            with the Gabor filter described by A, F and S to create the
%            filtered data Y. The Gabor filter is defined by the center
%            frequency F and by the bandwidth beta = A/S (that is the 
%            standard deviation of the gaussian). The infinite response
%            is truncated to N = (4/beta) + 1 points (each side).
%            [ note that S is the samplling frequency ].
%
%
%            [y, nftps, y1, h, h1] = GABORFILT(X, F, A, S) 
%
%            X   original signal (input)
%            Y   filtered signal (output)
%            F   center frequency of the Gabor filter
%            A   analogous to the bandwidth of the filter (typically 1000)
%            S   sampling frequency of the signal X   
%	     NFTPS filter length
%	     Y1 ?
%	     h gabor filter
%	     h1 gabor filter

function [y,nftps,y1,h,hl] = gaborfilt(x, f, a, s);


alpha=a;
center_freq=f;
sample_freq=s;
signal=x;

beta = alpha / sample_freq ;
Omeg_c = 2*pi*center_freq / sample_freq ;

gbw_up = alpha / sqrt(2*pi) ;
gdt_up = sqrt(pi/2) / alpha ;

N = ceil((4/beta) + 1) ;
beta2 = beta * beta;
nftps = 2 * N + 1 ;


n = [-N:1:N];
hl = exp(-beta2.*n.*n) ;
h = hl.*cos(n*Omeg_c) ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot the filter response in frequency

%clg;
%subplot(221);
%plot(h);
%magfth = abs(fft(h, 2048));
%plot(magfth(1:1024));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

y1 = conv(h,x);
sz = size(x);

y = y1(N+1:sz(2)+N);




             


