function  w = window(N,name,alpha);
% Synopsis: w = window(N,name,alpha).
% Generates a window.
% input parameters:
% N: the window length
% name: the window name inside quotes, as follows: 
%       'rect' for rectangular
%       'bart' for Bartlett
%       'hann' for Hann
%       'hamm' for Hamming
%       'blac' for Blackman
%       'kais' for Kaiser
%       'dolp' for Dolph
% alpha: the Kaiser window parameter for name = 'kais',
%        the side-lobe level (in dB) for name = 'dolp',
%        not used otherwise.
% Output:
% w: the window (a row vector)

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

n = 0:N-1;
if (name(1:4) == 'rect'),
   w = ones(1,N);
elseif (name(1:4) == 'bart'),
   w = 1-(1/(N+1))*abs(2*n-N+1);
elseif (name(1:4) == 'hann'),
   w = 0.5*(1-cos((2*pi/(N-1))*n));
elseif (name(1:4) == 'hamm'),
   w = 0.54-0.46*cos((2*pi/(N-1))*n);
elseif (name(1:4) == 'blac'),
   w = 0.42-0.5*cos((2*pi/(N-1))*n) + 0.08*cos((4*pi/(N-1))*n);
elseif (name(1:4) == 'kais'),
   w = 1/(besseli(0,alpha))* ...
       besseli(0,alpha*sqrt(1-((1/(N-1))*abs(2*n-N+1)).^2));
elseif (name(1:4) == 'dolp'),
   w = dolph(N,alpha);
else,
   error('Unrecognized window name in WINDOW')
end

