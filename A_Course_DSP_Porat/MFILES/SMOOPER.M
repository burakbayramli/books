function s = smooper(x,w);
% Synopsis: s = smooper(x,w).
% Computes the smoothed periodogram of the data vector x.
% Input parameters:
% x: the data vector
% w: the window; must have odd length.
% Output:
% s: the smoothed periodogram, of length equal to that of x.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

if (rem(length(w),2) == 0),
   error('Window in SMOOPER must have an odd length');
end
x = reshape(x,1,length(x));
x = x - mean(x);
kappa = (1/length(x))*conv(x,fliplr(x));
n = 0.5*(length(kappa)-length(w));
s = fft([zeros(1,n),w,zeros(1,n)].*kappa);
s = abs(s(1:length(x)));

