function y = fastfour(x)
%FASTFOUR Fast Fourier Transform.
%    fastfour(x) computes the same finite Fourier transform as FFT(x)
%    The code uses a recursive divide and conquer algorithm for
%    even order and matrix-vector multiplication for odd order.
%    If length(x) is m*n where m is even and n is odd, the
%    computational complexity of this approach is O(m*log(m))*O(n^2).

n = length(x);
i = sqrt(-1);
omega = exp(-2*pi*i/n);

if rem(n,2) == 0
   % Recursive divide and conquer
   k = (0:n/2-1)';
   w = omega .^ k;
   u = fastfour(x(1:2:n-1));
   v = w.*fastfour(x(2:2:n));
   y = [u+v; u-v];
else
   % Construct the Fourier matrix.
   j = 0:n-1;
   k = j';
   F = omega .^ (k*j);
   y = F*x;   
end






